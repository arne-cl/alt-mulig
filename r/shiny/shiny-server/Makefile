build:
	docker build -t shiny-server-test .

run:
	mkdir -p ~/shiny-logs
	(sleep 3 && xdg-open http://localhost:3838 > /dev/null 2>&1) &
	docker run --rm -p 3838:3838 -v ~/shiny-logs:/var/log/shiny-server:z shiny-server-test & sleep 3


