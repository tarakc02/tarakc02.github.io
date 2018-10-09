---
layout: default
class: default
title: TS - Home
---

<h1>TARAK SHAH</h1>
Hi I'm Tarak. I work as a data analyst, but this site is just for fiddling around. The [r packages](/projects) page has some R packages I've developed. I can't commit to a regular blog, but sometimes I do little [write-ups](/mathgames).

Where else I can be found:

<table class = "contact-info">
{% for contact in site.data.contact-info %}
<tr>
	<td class = "contact-type">{{contact.key}}</td>
	<td class = "contact-name"><a href = "{{contact.link}}">{{contact.name}}</a></td>
</tr>	
{% endfor %}
</table>
