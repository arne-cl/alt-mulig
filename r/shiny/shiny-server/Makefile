build:
	docker build -t shiny-server-test .

run:
	mkdir -p ~/shiny-logs
	xdg-open http://localhost:3838 &
	docker run --rm -p 3838:3838 -v ~/shiny-logs:/var/log/shiny-server shiny-server-test


