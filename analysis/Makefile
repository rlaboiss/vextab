sessions-list.md5: sessions-list.txt
	md5sum sessions-list.txt > sessions-list.md5

sessions-list.txt: FORCE
	./list-sessions.py > sessions-list.txt

.PHONY: FORCE
FORCE:

.PHONY: clean
clean:
	rm -f sessions-list.md5 sessions-list.txt
