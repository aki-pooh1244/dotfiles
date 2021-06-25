DRONES_DIR = $(shell git config "borg.drones-directory" || echo "lib")

-include $(DRONES_DIR)/borg/borg.mk

bootstrap-borg:
	@git submodule--helper clone --name borg --path lib/borg \
	--url git@github.com:emacscollective/borg.git
	@cd lib/borg; git symbolic-ref HEAD refs/heads/master
	@cd lib/borg; git reset --hard HEAD