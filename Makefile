CABAL_BIN_DIR := $(shell echo $$HOME)/.cabal/bin
PROJECT_NAME := hoz
EXE_NAME := hoz
VERSION := $(shell grep "^version:" $(PROJECT_NAME).cabal | awk '{print $$2}')
CABAL_RUN := cabal run exe:$(EXE_NAME) --

.PHONY: all build install test release clean

all: build

build:
	cabal build exe:$(EXE_NAME)

install:
	cabal install exe:$(EXE_NAME) --overwrite-policy=always

# Run regression tests and update outputs
# Use cabal run to avoid unnecessary global install or sdist during dev/test
test: build
	@echo "Running Ch1..."
	$(CABAL_RUN) examples/ctm-ch1.oz > examples/ctm-ch1.out 2>&1
	@echo "Running Ch2..."
	$(CABAL_RUN) examples/ctm-ch2.oz > examples/ctm-ch2.out 2>&1
	@echo "Running Ch3..."
	$(CABAL_RUN) examples/ctm-ch3.oz > examples/ctm-ch3.out 2>&1
	@echo "Running Ch4..."
	$(CABAL_RUN) -q 10 examples/ctm-ch4.oz > examples/ctm-ch4.out 2>&1
	@echo "All tests completed."

# Create a student distribution tarball
release: build
	@echo "Creating release tarball for $(PROJECT_NAME)-$(VERSION)..."
	rm -rf $(PROJECT_NAME)-$(VERSION)
	mkdir -p $(PROJECT_NAME)-$(VERSION)
	# Copy examples, docs, cabal file, Makefile, README, and update script
	cp -r examples docs $(PROJECT_NAME).cabal Makefile README.md update.sh $(PROJECT_NAME)-$(VERSION)/
	# Copy Haskell source files from root
	cp *.hs $(PROJECT_NAME)-$(VERSION)/
	# Copy scripts
	cp -r scripts $(PROJECT_NAME)-$(VERSION)/
	# Copy LICENSE if it exists
	-cp LICENSE $(PROJECT_NAME)-$(VERSION)/
	tar -czf $(PROJECT_NAME)-$(VERSION).tar.gz $(PROJECT_NAME)-$(VERSION)
	rm -rf $(PROJECT_NAME)-$(VERSION)
	@echo "Release tarball created: $(PROJECT_NAME)-$(VERSION).tar.gz"

clean:
	cabal clean
	rm -f $(EXE_NAME)
	rm -f reproduce_bug*.out
	rm -f $(PROJECT_NAME)-*.tar.gz

# Push the current branch to GitHub, then sync and publish it to the student main branch
publish:
	@echo "Publishing current branch..."
	@current_branch=$$(git branch --show-current); \
	if [ -z "$$current_branch" ]; then \
		echo "Not currently on any branch. Aborting."; \
		exit 1; \
	fi; \
	echo "Pushing active branch ($$current_branch) to origin..."; \
	git push origin "$$current_branch"; \
	echo "Merging $$current_branch into main and publishing..."; \
	git checkout main && \
	git pull origin main --rebase=false && \
	git merge "$$current_branch" && \
	git push origin main && \
	git checkout "$$current_branch"; \
	echo "Publish successful!"
