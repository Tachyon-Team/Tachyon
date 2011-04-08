/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

function z_char_to_html(c)
{
  if (c == 10 || c == 13)
    return "<br>";
  if (c == 32)
    return "&nbsp;";
  return "&#" + c + ";";
}

function z_string_to_html(str)
{
  var output = new Array();
  var i;
  for (i=0; i<str.length; i++)
    output.push(z_char_to_html(str.charCodeAt(i)));
  return output.join("");
}

function z_insert_child_at(parent,
                           index,
                           child)
{
  if (index !== false)
    {
      var c = z_child_at(parent, index);
      if (c)
        {
          parent.insertBefore(child, c);
          return;
        }
    }
  parent.appendChild(child);
}

function z_child_at(parent,
                    index)
{
  var c = parent.firstChild;
  var i = 0;
  while (c && i != index)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return c;
  return false2;
}

function z_child_index(child)
{
  var c = child.parentNode.firstChild;
  var i = 0;
  while (c && c !== child)
    {
      c = c.nextSibling;
      i++;
    }
  if (c)
    return i;
  return false2;
}

function z_get_pos(elem)
{
  return { x: elem.offsetLeft,
           y: elem.offsetTop };
}

function z_view_size(wind)
{
  if (wind.innerHeight !== undefined) // only IE does not support innerHeight
    return { x: wind.innerWidth,
             y: wind.innerHeight };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.clientHeight) // IE in strict mode
    return { x: de.clientWidth,
             y: de.clientHeight };

  var db = doc.body;

  if (db) // other IE
    return { x: db.clientWidth,
             y: db.clientHeight };

  return { x: 100, y: 100 };
}

function z_view_offset(wind)
{
  if (wind.pageYOffset !== undefined) // only IE does not support pageYOffset
    return { x: wind.pageXOffset,
             y: wind.pageYOffset };

  var doc = wind.document;
  var de = doc.documentElement;

  if (de && de.scrollTop) // IE in strict mode
    return { x: de.scrollLeft,
             y: de.scrollTop };

  var db = doc.body;

  if (db) // other IE
    return { x: db.scrollLeft,
             y: db.scrollTop };

  return { x: 0, y: 0 };
}

function z_get_frame_window(frame_id)
{
  var wind;
  if (window.frames && window.frames[frame_id]) // IE5, Konq, Safari
    wind = window.frames[frame_id];
  else
    {
      var elem = document.getElementById(frame_id);
      if (elem.contentWindow) // IE5.5+, Moz 1.0+, Opera
        wind = elem.contentWindow;
      else // Moz < 0.9 (Netscape 6.0)
        wind = elem;
    }
  return wind;
} 

function z_get_frame_document(frame_id)
{ 
  var wind = z_get_frame_window(frame_id);
  var doc = wind.document;
  if (!doc)
    doc = wind.contentDocument;
  // Moz 0.9+, Konq, Safari, IE, Opera||Moz < 0.9 (NS 6.0) 
  return doc;
}

function z_get_document(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow.document;

  if (ifrm.contentDocument) // NS6
    return ifrm.contentDocument;

  if (ifrm.document) // IE5
   return ifrm.document;

  return null;
}

function z_get_window(ifrm)
{
  if (ifrm.contentWindow) // IE5.5+
    return ifrm.contentWindow;

  return frames[ifrm.id];
}

var z_ic = null;

function z_input_controller()
{
  var keydown = 0;
  var keypress = 1;
  var keyup = 2;

  var keydown_code = -1;
  var keydown_processed = false;
  var input_handler = null;

  function set_input_handler(ih)
  {
    if (input_handler)
      input_handler.handle_blur();
    input_handler = ih;
    if (input_handler)
      input_handler.handle_focus();
  }

  function make_key_handler(type)
  {
    function handler(e)
    {
      if (!e) e = window.event;

      var code = e.keyCode;
      if (!code) code = e.which;

      switch (type)
        {
          case keydown:
            if ((code >= 1 && code <= 31) ||    // backspace/tab/enter/...
                (code >= 33 && code <= 40) ||   // pgup/pgdn/end/home/arrows
                (code >= 45 && code <= 46) ||   // ins/del
                (code >= 112 && code <= 123))   // F1/.../F12
              {
                keydown_code = code;
                keydown_processed = true;
                input_handler.handle_key(keydown_code,keydown_code);
              }
            else
              keydown_code = -1;
            break;

          case keypress:
            if (input_handler)
              {
                if (keydown_code >= 0)
                  {
                    if (keydown_processed)
                      keydown_processed = false;
                    else
                      input_handler.handle_key(keydown_code,keydown_code);
                  }
                else if (code < 128) // IE generates a keypress of 191 on /
                  {
                    if (e.ctrlKey)
                      {
                        if (code >= 64 && code <= 95)
                          code = code - 64;
                        else if (code >= 96 && code <= 127)
                          code = code - 96;
                        else if (code == 32)
                          code = 0;
                        else if (code > 32)
                          code = -1;
                      }
                    if (code >= 0)
                      input_handler.handle_key(code,keydown_code);
                  }
              }
            break;
        }

      if (e.stopPropagation)
        e.stopPropagation();
      e.cancelBubble = true;

      if (type == keydown && keydown_code < 0)
        return true;
      else
        return false;
    }

    return handler;
  }

  this.set_input_handler = set_input_handler;
  this.onkeydown = make_key_handler(keydown);
  this.onkeypress = make_key_handler(keypress);
  this.onkeyup = make_key_handler(keyup);
}

