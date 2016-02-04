PWD = $(shell pwd)
REPO ?= ague
PKG_REVISION ?= $(shell git describe --tags)
PKG_VERSION ?= $(shell git describe --tags | tr - .)
PKG_ID = cinched-$(PKG_VERSION)
PKG_BUILD = 1

# No hyphens are allowed in the _version field in RPM
PKG_VERSION_NO_H ?= $(shell echo $(PKG_VERSION) | tr - .)

default:
	rm -rf $(PKG_ID)/
	GIT_DIR=../.git git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION) | tar -xf -
	cd $(PKG_ID)/ && rebar -C rebar.config.lock get-deps
	cd  $(PKG_ID)/ && for dep in deps/*; do \
                echo "Processing dep: $${dep}"; \
                mkdir -p $${dep}/priv; \
                git --git-dir=$${dep}/.git describe --tags >$${dep}/priv/vsn.git  || true; \
        done
	find $(PKG_ID)/ -depth -type d -name ".git" -exec rm -rf {} \;
	tar -czf $(PKG_ID).tar.gz $(PKG_ID)
	mkdir -p BUILD
	mkdir -p packages
	rpmbuild --sign --define "_rpmfilename %%{NAME}-%%{VERSION}-%%{RELEASE}.%%{ARCH}.rpm" \
	--define '_topdir $(PWD)' \
	--define '_sourcedir $(PWD)' \
	--define '_specdir $(PWD)' \
	--define '_rpmdir $(PWD)/packages' \
	--define '_srcrpmdir $(PWD)/packages' \
	--define "_revision $(PKG_VERSION)" \
	--define "_version $(PKG_VERSION_NO_H)" \
	--define "_release $(PKG_BUILD)" \
	--define "_tarname $(PKG_ID).tar.gz" \
	--define "_tarname_base $(PKG_ID)" \
	-ba $(PWD)/specfile
	cd packages && \
	for rpmfile in *.rpm; do \
	sha256sum $${rpmfile} > $${rpmfile}.sha \
	; done
