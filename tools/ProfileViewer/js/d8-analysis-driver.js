// print("Starting...");

onmessage({
    'data' : 'load ' + read(arguments[0])
})

tachyon_print(JSON.stringify(jsonData));