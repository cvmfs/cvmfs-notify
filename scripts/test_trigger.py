#!/usr/bin/env python3

import requests

host_name = 'localhost'
port = 8081
api_root = '/api/v1'

url = 'http://' + host_name + ':' + str(port) + api_root + '/trigger'

def do_trigger(repo, revision, root_hash):
    print('Trigger: ', repo, revision, root_hash)
    data = {'repo' : repo,
            'revision' : revision,
            'root_hash' : root_hash,
            'api_version' : 1}
    headers = {'Content-type' : 'application/json'}
    reply = requests.post(url, json = data, headers = headers)
    print('Reply: status: {}, json: {}'.format(reply.status_code, reply.json()))

def main():
    do_trigger('test_repo', 5, 'abcdefg')

if __name__ == '__main__':
    main()
