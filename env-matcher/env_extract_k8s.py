#!/usr/bin/env python3

import argparse
import yaml
import sys


def get_docs_from_yamlfile(yaml_filepath):
    with open(yaml_filepath, 'r') as yfile:
        ydocs = yaml.load_all(yfile)
        return [doc for doc in ydocs]


def get_deployment_doc(yaml_docs):
    for ydoc in yaml_docs:
        kind = ydoc.get('kind')
        if kind in ('Deployment', 'DaemonSet'):
            return ydoc
    raise ValueError("No 'Deployment' doc in YAML file.")


def get_env_dict(deployment_doc):
    env_dict = {}
    envs = deployment_doc['spec']['template']['spec']['containers'][0]['env']
    for env in envs:
        env_dict[env['name']] = env['value']
    return env_dict


def get_envs_from_yamlfile(yaml_file):
    yaml_docs = get_docs_from_yamlfile(yaml_file)
    deployment_doc = get_deployment_doc(yaml_docs)
    return get_env_dict(deployment_doc)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('k8s_file', help="Kubernetes YAML config file to extract ENVs from")
    parser.add_argument(
        '-s', '--sort', dest='sort', default=False, action='store_true',
        help="Sort the ENVs alphabetically (ascending, default: False).")

    args = parser.parse_args(sys.argv[1:])
    env_dict = get_envs_from_yamlfile(args.k8s_file)

    if args.sort:
        for key in sorted(env_dict.keys()):
            print("{0}={1}".format(key, env_dict[key]))
    else:
        for key, val in env_dict.items():
            print("{0}={1}".format(key, val))
