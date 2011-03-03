importScripts('v8/splaytree.js',
    'v8/codemap.js',
    'v8/csvparser.js',
    'v8/consarray.js',
    'v8/profile.js',
    'v8/profile_view.js',
    'v8/logreader.js',
    'v8/tickprocessor.js',
    'v8/tickprocessor-driver.js'
);

var logData;
var currentPos;
var tickProcessor;

function print() {

}

function readline() {
    if (currentPos > logData.length) return null;
    
    var i = logData.indexOf("\n", currentPos);
    if (i < 0) i = logData.length;    
    var s = logData.slice(currentPos, i);
    currentPos = i + 1;
    return s;
}

function analyze() {        
    var args = [ "--mac" ];
    currentPos = 0;
    tickProcessor = makeTickProcessor(args);
    logData = undefined;
}

function computeStats() {
    var ticks = tickProcessor.ticks_;
    var data = new Array(
            ["Program", ticks.total - ticks.unaccounted - ticks.excluded - ticks.gc],
            ["GC", ticks.gc]);
    if (ticks.unaccounted > 0) {
        data.push(["Unaccounted", ticks.unaccounted]);
    }
    if (ticks.excluded > 0) {
        data.push(["Excluded", ticks.excluded]);
    }
    return data;
    
//    return new Array([ "Program", 50], ["GC", 20], ["Other", 35]);
}

function getTotalTicks() {
    var ticks = tickProcessor.ticks_;
    var total = ticks.total;
    if (tickProcessor.ignoreUnknown_) {
        total -= ticks.unaccounted;
    }
    return total;
}

function computeFlatProfile(filterFunc) {
    var flatProfile = tickProcessor.profile_.getFlatProfile();
    var flatView = tickProcessor.viewBuilder_.buildView(flatProfile);
    flatView.sort(function(rec1, rec2) {
        return rec2.selfTime - rec1.selfTime ||
            (rec2.internalFuncName < rec1.internalFuncName ? -1 : 1); });
            
    var totalTicks = getTotalTicks();
    flatView.head.totalTime = totalTicks;

    // Count library ticks
    var flatViewNodes = flatView.head.children;
    var libraryTicks = 0;
    tickProcessor.processProfile(flatViewNodes,
        function(name) { return tickProcessor.isSharedLibrary(name); },
        function(rec) { libraryTicks += rec.selfTime; });
    var nonLibraryTicks = totalTicks - libraryTicks;
    
    var result = {
        'headers' : ["ticks", "total", "nonLib", "name"],
        'data' : [],
    };
    
    var filters = new Array(
        ['js', function (name) { return tickProcessor.isJsCode(name); }],
        ['libs', function (name) { return tickProcessor.isSharedLibrary(name); }],
        ['cpp', function (name) { return tickProcessor.isCppCode(name); }]
    );
    
    for (var i = 0; i < filters.length; i++) {        
        var data = [];
        var name = filters[i][0];
        var fn = filters[i][1];
        populateFlatProfile(flatViewNodes, nonLibraryTicks, data, fn);
        result.data.push({
            'name' : name,
            'data' : data
        });
    }    
    
    return result;
}

function populateFlatProfile(nodes, nonLibraryTicks, data, filterFunc) {
    tickProcessor.processProfile(nodes, filterFunc, function (rec) {
        if (rec.selfTime == 0) return;
        data.push([rec.selfTime, rec.selfPercent / 100, rec.selfTime / nonLibraryTicks,  rec.internalFuncName]);
    });
}

function computeBottomUpProfile() {
    var heavyProfile = tickProcessor.profile_.getBottomUpProfile();
    var heavyView = tickProcessor.viewBuilder_.buildView(heavyProfile);
    
    // To show the same percentages as in the flat profile.
    heavyView.head.totalTime = getTotalTicks();
    
    heavyView.sort(function(rec1, rec2) {
          return rec2.totalTime - rec1.totalTime ||
              (rec2.internalFuncName < rec1.internalFuncName ? -1 : 1); });
    
    var headers = ["ticks", "parent", "name"];
    var prof = [];
    populateBottomUpProfile(heavyView.head.children, prof);
    
    return {
        'headers' : headers,
        'profile' : prof
    };
}

function populateBottomUpProfile(profile, parent, level) {
    if (level === undefined) level = 0;
    tickProcessor.processProfile(profile, function() { return true; }, function (rec) {
        // Cut off too infrequent callers.
        if (rec.parentTotalPercent < TickProcessor.CALL_PROFILE_CUTOFF_PCT) return;
        
        var node = {
            'data' : [rec.totalTime, rec.parentTotalPercent, rec.internalFuncName],
            'children' : undefined
        }        
        
        // Limit backtrace depth
        if (level < 5) {
            if (rec.children && rec.children.length > 0) {
                var children = [];
                populateBottomUpProfile(rec.children, children, level + 1);
                node.children = children;
            }
        }
        
        parent.push(node);
      });
}

function computeTopDownProfile() {
    var topDownProfile = tickProcessor.profile_.getTopDownProfile();
    var topDownView = tickProcessor.viewBuilder_.buildView(topDownProfile);
    topDownView.head.totalTime = getTotalTicks();

    topDownView.sort(function(rec1, rec2) {
      return rec2.totalTime - rec1.totalTime ||
          (rec2.internalFuncName < rec1.internalFuncName ? -1 : 1); });
    
    var headers = ["ticks", "parent", "name"];
    var prof = [];
    populateTopDownProfile(topDownView.head.children, prof);
    
    return {
        'headers' : headers,
        'profile' : prof
    };
}

function populateTopDownProfile(profile, parent, level) {
    if (level === undefined) level = 0;

    tickProcessor.processProfile(profile, function() { return true; }, function (rec) {
        if (rec.parentTotalPercent < TickProcessor.CALL_PROFILE_CUTOFF_PCT) return;
        // if (rec.selfPercent < 0.01) return;

        var node = {
            'data' : [rec.totalTime, rec.parentTotalPercent, rec.internalFuncName],
            'children' : undefined
        }
        
        if (rec.children && rec.children.length > 0) {
            var children = [];
            populateTopDownProfile(rec.children, children, level + 1);
            node.children = children;
        }
        
        parent.push(node);
      });
}

function postStats() {
    postMessage({
        'kind' : 'stats',
        'data' : computeStats()
    });
}

function postFlatProfile() {
    postMessage({
        'kind' : 'flat',
        'data' : computeFlatProfile()
    });
}

function postBottomUpProfile() {
    postMessage({
        'kind' : 'bottomUp',
        'data' : computeBottomUpProfile()
    });
}

function postTopDownProfile() {
    postMessage({
        'kind' : 'topDown',
        'data' : computeTopDownProfile()
    });
}

onmessage = function(event) {
     // tachyon_print("Event received");
     var req = event.data;
     if (req.substr(0, 4) === "load") {
         logData = req.slice(5);
         analyze();
         postMessage({'kind' : 'done'});
         postStats();
         postFlatProfile();
         postBottomUpProfile();
         postTopDownProfile();
     } else if (req === "stats") {
         postStats();
     }     
 }
