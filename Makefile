
all :
	stack build

run :
	stack exec ascii-tesselations-exe

spec :
	stack build ascii-tesselations:ascii-tesselations-test

spec-watch :
	while true; \
	do \
		change=$$(inotifywait -e close_write,moved_to,create app src test) \
		make spec; \
	done;

watch :
	while true; \
	do \
		change=$$(inotifywait -e close_write,moved_to,create app src) \
		make all && make run; \
	done;

