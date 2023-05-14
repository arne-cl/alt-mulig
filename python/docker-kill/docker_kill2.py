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

    container_info = []
    for container in containers:
        container_info.append({
            'name': container.name,
            'id': container.id[:12],
            'image': container.image.tags[0],
            'status': container.status,
            'ports': container.attrs['NetworkSettings']['Ports']
        })

    click.echo("Running containers:")
    for info in container_info:
        click.echo(f" - Name: {info['name']}, ID: {info['id']}, Image: {info['image']}, Status: {info['status']}, Ports: {info['ports']}")

    question = [
        inquirer.List(
            "selected_container_id",
            message="Select a container to kill",
            choices=[info['id'] for info in container_info],
        )
    ]

    answer = inquirer.prompt(question)
    selected_container_id = answer["selected_container_id"]

    container_to_kill = None
    for container in containers:
        if container.id.startswith(selected_container_id):
            container_to_kill = container
            break

    if container_to_kill is not None:
        container_to_kill.kill()
        click.echo(f"Container '{selected_container_id}' killed.")

if __name__ == "__main__":
    docker_kill()
