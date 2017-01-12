---
layout: default
class: projects
title: TS - Projects
permalink: /projects/
---

Here's some things I've been working on recently:

<ul class = "project-list">
{% for project in site.data.projects %}
	<li><a href="https://github.com/{{ project.github }}">{{project.name}}</a>: {{ project.description }}</li>
{% endfor %}
</ul>
