obj-stab-psycho.csv: obj-stab-resp.csv psychometric-estimates.r
	Rscript psychometric-estimates.r

obj-stab-resp.csv: sessions-list.txt
	if [ "$$(md5sum sessions-list.txt)" != "$$(cat sessions-list.md5)" ] ; then	\
	    ./build-database.py ;							\
	    md5sum sessions-list.txt > sessions-list.md5 ;				\
	fi

sessions-list.txt: FORCE
	./list-sessions.py > sessions-list.txt

.PHONY: FORCE
FORCE:

.PHONY: clean
clean:
	rm -f sessions-list.md5 sessions-list.txt
