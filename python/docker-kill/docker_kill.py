#!/usr/bin/env python3

# pip install click docker prompt_toolkit

import click
import docker
import inquirer
import time

@click.command()
def docker_kill():
    client = docker.from_env()
    containers = client.containers.list()

    if not containers:
        click.echo("No running containers found.")
        return

    container_names = [container.name for container in containers]

    click.echo("Running containers:")
    for name in container_names:
        click.echo(f" - {name}")

    question = [
        inquirer.List(
            "selected_container_name",
            message="Select a container to kill",
            choices=container_names,
        )
    ]

    answer = inquirer.prompt(question)
    selected_container_name = answer["selected_container_name"]

    container_to_kill = None
    for container in containers:
        if container.name == selected_container_name:
            container_to_kill = container
            break

    if container_to_kill is not None:
        container_to_kill.kill()
        click.echo(f"Container '{selected_container_name}' killed.")

if __name__ == "__main__":
    docker_kill()
