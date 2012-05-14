function showBubble(divId, spanId)
{
    var div = document.getElementById(divId);
    var span = document.getElementById(spanId);

    var rect = span.getBoundingClientRect();

    div.style.visibility = 'visible';

    div.style.top  = document.body.scrollTop + rect.top + rect.height + 5;
    div.style.left = document.body.scrollLeft + rect.left + 5;
}

function hideBubble(divId)
{
    var div = document.getElementById(divId);

    div.style.visibility = 'hidden';
}
