#!/usr/bin/env python3

# pip install click docker inquirer tabulate

import click
import docker
import inquirer
from tabulate import tabulate

@click.command()
def docker_kill():
    client = docker.from_env()
    containers = client.containers.list()

    if not containers:
        click.echo("No running containers found.")
        return

    container_info = []
    for container in containers:
        ports = container.attrs['NetworkSettings']['Ports']
        port_mappings = []
        for port, mapping in ports.items():
            if mapping:
                for m in mapping:
                    port_mappings.append(f"{m['HostIp']}:{m['HostPort']}->{port}")
            else:
                port_mappings.append(port)

        container_info.append({
            'name': container.name,
            'id': container.id[:12],
            'image': container.image.tags[0],
            'status': container.status,
            'ports': ', '.join(port_mappings)
        })

    table_headers = ["CONTAINER ID", "IMAGE", "STATUS", "PORTS", "NAMES"]
    table_data = [[info['id'], info['image'], info['status'], info['ports'], info['name']] for info in container_info]

    click.echo(tabulate(table_data, headers=table_headers, tablefmt="grid"))

    question = [
        inquirer.List(
            "selected_container_name",
            message="Select a container to kill",
            choices=[info['name'] for info in container_info],
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
