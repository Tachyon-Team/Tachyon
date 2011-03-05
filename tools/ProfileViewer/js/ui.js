
function makeBar(w) {
    // <div class="bar-wrapper"><div style="width: 50px;" class="bar ui-widget-header"></div><span class="bar-label">50%</span></div>
    var wrapper = document.createElement('div');
    wrapper.setAttribute('class', 'bar-wrapper');
    
    var bar = document.createElement('div');
    bar.setAttribute('style', 'width: ' + (w * 100).toFixed(0) + 'px;');
    bar.setAttribute('class', 'bar ui-widget-header');
    wrapper.appendChild(bar);
    
    var lbl = document.createElement('span');
    lbl.setAttribute('class', 'bar-label');
    lbl.appendChild(document.createTextNode((w * 100).toFixed(1) + "%"));
    wrapper.appendChild(lbl);
    
    return wrapper;
}

function outputStats(containerID, data) {
    $('#' + containerID).innerHTML = "";
    var chart = new JSChart(containerID, 'pie');
    // var colors = [ "#e79c00", "#edad38", "#eeb64d", "#f0bc59" ]; // Orange monochrome
    var colors = ["#1f8bd2", "#69b3e5", "#8cc7ef", "#c1e6ff"]; // Blue monochrome
    chart.setDataArray(data);
    chart.colorizePie(colors.slice(0, data.length));
    chart.setSize(250, 250);
    chart.setTitle('Ticks');
    chart.draw();
}

function outputFlatProfile(container, headers, data) {
    var tbl = document.createElement('table');
    var i, row;
    
    row = document.createElement('tr');
    row.setAttribute('class', 'hover');
    for (i = 0; i < headers.length; i++) {
        if (i === 2) continue; // Skip nonLib
        var cell = document.createElement('th');
        cell.appendChild(document.createTextNode(headers[i]));
        row.appendChild(cell);
    }
    tbl.appendChild(row);
    
    for (i = 0; i < data.length; i++) {
        var record = data[i];
        row = document.createElement('tr');
        
        var cell;
        
        // Ticks
        cell = document.createElement('td');
        cell.appendChild(document.createTextNode(record[0]));
        row.appendChild(cell);
        
        // Total
        cell = document.createElement('td');
        cell.appendChild(makeBar(record[1]));
        row.appendChild(cell);
        
        // Non-lib
        // cell = document.createElement('td');
        // cell.appendChild(makeBar(record[2]));
        // row.appendChild(cell);
        
        // Name
        cell = document.createElement('td');
        cell.appendChild(document.createTextNode(record[3]));
        cell.setAttribute("class", "code");
        row.appendChild(cell);
          
        tbl.appendChild(row);
    }
    // var container = document.getElementById('flat-js');
    container.innerHTML = "";
    container.appendChild(tbl);
    
    /* BD -- disabled for now
    $(tbl).delegate('td','mouseover mouseleave', function(e) {
        if (e.type == 'mouseover') {
          $(this).parent().addClass("hover");
          $(this).prev().children('div.bar-wrapper')[0].addClass("hover");
          // $("colgroup").eq($(this).index()).addClass("hover");
        }
        else {
          $(this).parent().removeClass("hover");
          $(this).prev().children('div.bar-wrapper')[0].removeClass("hover");
          // $("colgroup").eq($(this).index()).removeClass("hover");
        }
    });
    */
}

function outputTreeProfile(container, data, newID) {
    var tblbody = document.createElement('tbody');
    
    var i, row;
    
    row = document.createElement('tr');
    var myId = newID();
    row.setAttribute('id', myId);
    for (i = 0; i < data.headers.length; i++) {
        var cell = document.createElement('th');
        cell.appendChild(document.createTextNode(data.headers[i]));
        row.appendChild(cell);
    }
    tblbody.appendChild(row);
    
    for (i = 0; i < data.profile.length; i++) {            
        var record = data.profile[i];
        outputTree(record, tblbody, newID);
    }

    var tbl = document.createElement('table');
    tbl.appendChild(tblbody);
    
    container.innerHTML = "";
    container.appendChild(tbl);
    
    $(tbl).treeTable({
        treeColumn : 2,
        clickableNodeNames : true
    });
    // $("#treedemo").treeTable();
}

function outputTree(record, tbl, newID, parentId) {
    // Output self
    row = document.createElement('tr');
    var myId = newID();
    row.setAttribute('id', myId);
    if (parentId) {
        row.setAttribute('class', 'child-of-' + parentId); 
    }
    
    var cell;
    
    // Ticks
    cell = document.createElement('td');
    cell.appendChild(document.createTextNode(record.data[0]));
    row.appendChild(cell);
    
    // Parent
    cell = document.createElement('td');
    cell.appendChild(makeBar(record.data[1] / 100));
    row.appendChild(cell);
    
    // Name
    cell = document.createElement('td');
    cell.appendChild(document.createTextNode(record.data[2]));
    cell.setAttribute("class", "code");
    row.appendChild(cell);
    
    tbl.appendChild(row);
    
    // Output children
    if (record.children) {
        for (var i = 0; i < record.children.length; i++) {
            outputTree(record.children[i], tbl, newID, myId);
        }
    }
}