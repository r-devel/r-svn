
const macros = {
    "\\R": "\\textsf{R}",
    "\\mbox": "\\text",
    "\\code": "\\texttt"
};

function processMathHTML()
{
    var l = document.getElementsByClassName('reqn');
    for (let i = 0; i < l.length; i++) {
	let e= l[i];
	katex.render(e.textContent, e,
		     {
			 throwOnError: false,
			 macros: macros
		     });
    }
    return;
}

