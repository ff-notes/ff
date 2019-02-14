#!/usr/bin/env python3

from subprocess import check_call, check_output

core_packages = """
    array
    binary
    bytestring
    clock
    containers
    deepseq
    directory
    filepath
    ghc-boot-th
    ghc-prim
    hashable
    integer-gmp
    mtl
    parsec
    pretty
    primitive
    process
    rts
    stm
    template-haskell
    terminfo
    text
    time
    transformers
    transformers-compat
    unix
    unordered-containers
    vector
""".split()

really_used_packages = [
    "github",  # to track github issues
]

dependency_graph = check_output([
    'stack', 'dot',
    '--external',
    '--no-include-base',
    '--prune=' + ','.join(core_packages),
])

dependency_graph = dependency_graph[0:-2]
for package in really_used_packages:
    dependency_graph += '"{}" [fillcolor = green, style = filled];'.format(package).encode()
dependency_graph += b'\n}\n'

# print(dependency_graph)

image = check_output(['dot', '-Tpng'], input=dependency_graph)

with open('/tmp/ff.png', 'wb') as f:
    f.write(image)

check_call(['xdg-open', '/tmp/ff.png'])
