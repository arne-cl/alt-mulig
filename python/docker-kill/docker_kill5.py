#!/usr/bin/env python3

import click
import docker
import inquirer
from tabulate import tabulate


def get_running_containers():
    """Get a list of running containers with their information."""
    client = docker.from_env()
    containers = client.containers.list()

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

        image_tag = container.image.tags[0] if container.image.tags else "unknown"

        container_info.append({
            'name': container.name,
            'id': container.id[:12],
            'image': image_tag,
            'status': container.status,
            'ports': ', '.join(port_mappings)
        })

    return container_info



def display_containers(container_info):
    """Display container information in a table format."""
    table_headers = ["CONTAINER ID", "IMAGE", "STATUS", "PORTS", "NAMES"]
    table_data = [[info['id'], info['image'], info['status'], info['ports'], info['name']] for info in container_info]

    click.echo(tabulate(table_data, headers=table_headers, tablefmt="grid"))


def prompt_user(container_info):
    """Prompt the user to select a container or an action."""
    choices = [info['name'] for info in container_info] + ["kill all containers", "kill no containers"]

    question = [
        inquirer.List(
            "selected_container_name",
            message="Select a container to kill",
            choices=choices,
        )
    ]

    try:
        answer = inquirer.prompt(question)
        return answer["selected_container_name"]
    except KeyboardInterrupt:
        click.echo("\nOperation canceled.")
        return None


def kill_selected_container(selected_container_name, containers):
    """Kill the selected container or perform the chosen action."""
    if selected_container_name is None:
        return

    if selected_container_name == "kill no containers":
        click.echo("No container was killed.")
        return

    if selected_container_name == "kill all containers":
        for container in containers:
            container.kill()
            click.echo(f"Container '{container.name}' killed.")
        return

    container_to_kill = None
    for container in containers:
        if container.name == selected_container_name:
            container_to_kill = container
            break

    if container_to_kill is not None:
        container_to_kill.kill()
        click.echo(f"Container '{selected_container_name}' killed.")


@click.command()
def docker_kill():
    """List running containers and allow the user to kill a selected container."""
    container_info = get_running_containers()

    if not container_info:
        click.echo("No running containers found.")
        return

    display_containers(container_info)

    selected_container_name = prompt_user(container_info)

    containers = docker.from_env().containers.list()
    kill_selected_container(selected_container_name, containers)


if __name__ == "__main__":
    docker_kill()
