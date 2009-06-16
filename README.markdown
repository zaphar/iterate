README
======

Iterate! is a scrum style agile story/backlog/iteration manager.
It is intended to do the work of such tools as XPlanner or Pivotal Tracker.

INSTALLING
==========

Download the source to desired location, then:

	git submodule init
	git submodule update
	make
	make boot
	./scripts/setup.erl
	./start.sh

Make sure that you have the same `-name' argument in both /scripts/setup.erl and your start.sh scripts!
Otherwise you won't have any Mnesia tables.
 
CREDITS
=======

2008-2009 Jeremy Wall<br />
