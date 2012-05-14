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

function toggleBody(bodyId, buttonId)
{
    var bodySpan = document.getElementById(bodyId);
    var buttonSpan = document.getElementById(buttonId);

    if (bodySpan.style.display === 'none')
    {
        bodySpan.style.display = 'block';
        buttonSpan.innerHTML = '[-]';
    }
    else
    {
        bodySpan.style.display = 'none';
        buttonSpan.innerHTML = '[+]';
    }
}

