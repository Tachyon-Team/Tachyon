function getPos(obj)
{
    var curLeft = 0;
    var curTop = 0;

    if (obj && obj.offsetParent) 
    {
	    do 
        {
	         curLeft += obj.offsetLeft;
	         curTop += obj.offsetTop;
        } 
        while (obj = obj.offsetParent);
    }

    return { left: curLeft, top: curTop };
}

function show(divId, spanId)
{
    var span = document.getElementById(spanId);
    var div = document.getElementById(divId);

    var spanPos = getPos(span);

    div.style.visibility = 'visible';

    div.style.top = spanPos.top + span.offsetHeight + 5;
    div.style.left = spanPos.left + 5;
}

function hide(divId)
{
    var div = document.getElementById(divId);

    div.style.visibility = 'hidden';
}
