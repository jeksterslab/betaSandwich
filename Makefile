.PHONY: build all git ssh term termtmux termxterm dotfiles project pkg tinytex clean cleanpkg cleantinytex cleanall coverage lint

build: pkg clean
	@echo TinyTex...
	@Rscript -e "rProject::TinyTex(\"${PWD}\", force = FALSE)"
	@echo Styling...
	@Rscript -e "rProject::Style(\"${PWD}\")"
	@echo Linting...
	@Rscript -e "rProject::Lint(\"${PWD}\")"
	@echo Building dependencies...
	@Rscript -e "rProject::DataProcess(\"${PWD}\")"
	@Rscript -e "rProject::DataAnalysis(\"${PWD}\")"
	@Rscript -e "rProject::Dependencies(\"${PWD}\")"
	@echo Initial build...
	@Rscript -e "rProject::Build(\"${PWD}\")"
	@echo Precompiling vignettes...
	@Rscript -e "rProject::VignettesPrecompile(\"${PWD}\")"
	@echo Building project...
	@Rscript -e "rProject::Build(\"${PWD}\")"
	@echo Building website...
	@Rscript -e "rProject::Site(\"${PWD}\")"
	@echo Building manual...
	@Rscript -e "rProject::Manual(\"${PWD}\", project = Sys.getenv(\"PROJECT\"))"
	@echo Building CITATION.cff...
	@Rscript -e "rProject::CFF(\"${PWD}\")"

all: git ssh term build latex

cleanall: clean cleanpkg cleantinytex

git:
	@echo Setting git...
	@(cd .setup/git && make)

ssh:
	@echo Setting ssh keys...
	@(cd .setup/ssh && make)

term:
	@echo Building .bashrc...
	@(cd .setup/bash && make)
	@echo Building .vimrc...
	@(cd .setup/vim && make)

termtmux: term project
	@Rscript -e "rProject::Tmux()"
	@echo Building .tmux.conf...
	@(cd .setup/tmux && make)

termxterm: term
	@echo Building .Xresources...
	@(cd .setup/xterm && make)

dotfiles:
	@echo Building dotfiles...
	@bash .dotfiles

project:
	@echo Building project...
	@Rscript make-project.R ${PWD}

pkg: project
	@echo Installing packages...
	@Rscript make-packages.R ${PWD}

tinytex:
	@echo Installing TinyTex...
	@Rscript -e "rProject::TinyTex(\"${PWD}\", force = TRUE)"

clean:
	@echo Cleaning...
	@Rscript -e "rProject::Clean(\"${PWD}\")"

cleanpkg:
	@echo Cleaning packages...
	@Rscript -e "rProject::CleanPkg(\"${PWD}\")"

cleanproj:
	@echo Cleaning project...
	@Rscript -e "rProject::CleanProj(\"${PWD}\")"

cleantinytex:
	@echo Cleaning TinyTex...
	@Rscript -e "rProject::CleanTinyTex(\"${PWD}\")"

coverage:
	@echo Code coverage...
	@Rscript -e "rProject::Coverage(\"${PWD}\")"

lint:
	@echo Styling...
	@Rscript -e "rProject::Style(\"${PWD}\")"
	@echo Linting...
	@Rscript -e "rProject::Lint(\"${PWD}\")"

latex:
	@Rscript -e "rProject::LatexMake(\"${PWD}\")"

# make all
# make git
# make ssh
# make term
# make termtmux
# make termxterm
# - for local build
# - not for git clone
