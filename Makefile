HAS_ELIXIR=1

include bu.mk

elixir::
	$(verbose) $(MKDIR_P) lib
	$(verbose) $(CP_R) exlib/* lib

