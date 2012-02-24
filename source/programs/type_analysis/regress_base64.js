var data = 'ABCDEF';

data.charCodeAt(0);

var result = data[data.charCodeAt(0) & 0x3f];

typeAssert(result, '"string"');

