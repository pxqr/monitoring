var styleCookie = "style";

function setStyle(title)
{
    var links = document.getElementsByTagName("link");
    for (var i = 0; i < links.length; ++i) {
        var link = links[i];
        if ((link.rel.indexOf("stylesheet") != -1) && link.title) {
            link.disabled = true;
            if (link.title == title) {
                link.disabled = false;
            }
        }
    }
    setCookie(styleCookie, title);
}

function setStyleFromCookie()
{
    var styleName = getCookie(styleCookie);
    if (styleName) {
        setStyle(styleName);
    }
}

function getCookie(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');

    for(var i = 0; i < ca.length; i++) {
        var c = ca[i];

        while (c.charAt(0) == ' ') {
            c = c.substring(1,c.length);
        }
        if (c.indexOf(nameEQ) == 0) {
            return unescape(c.substring(nameEQ.length,c.length));
        }
    }
    return null;
}

function setCookie(name, value)
{
    document.cookie = name + "=" + value + ";";
}

function loadPage()
{
    setStyleFromCookie();
    runMonitor(4000);
}