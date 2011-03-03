function importScripts() {
    // Do nothing
}

var tachyon_print = print;

var jsonData = {
    kind : 'multi',
    data : []
};

function postMessage(msg) {
    // tachyon_print("Posting: " + msg.kind);
    // tachyon_print(JSON.stringify(x.data));
    jsonData.data.push(msg);
}