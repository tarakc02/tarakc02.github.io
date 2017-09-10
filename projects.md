---
layout: default
class: default
title: TS - Projects
---

<h1>SOFTWARE THINGS</h1>

<ul class = "project-list">
{% for project in site.data.projects %}
	<li><a href="https://github.com/{{ project.github }}">{{project.name}}</a>: {{ project.description }}</li>
{% endfor %}
</ul>
