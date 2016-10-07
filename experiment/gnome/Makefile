APPDIR = $(HOME)/.local/share/applications
ICON = object-stability.svg
ICONDIR = $(HOME)/.local/share/icons
EXEC = $(shell cd .. ; pwd)/interface.py

.PHONY: install
install: object-stability.desktop
	mkdir -p $(APPDIR)
	install --mode=755 object-stability.desktop $(APPDIR)
	mkdir -p $(ICONDIR)
	install --mode=644  $(ICON) $(ICONDIR)
	gsettings set org.gnome.shell favorite-apps	\
	    "['object-stability.desktop',		\
	      'org.gnome.Terminal.desktop',		\
	      'org.gnome.Nautilus.desktop',		\
	      'chromium.desktop']"

%: %.in
	sed -e 's|#EXEC#|$(EXEC)|;s|#ICON#|$(ICONDIR)/$(ICON)|' < $< > $@
