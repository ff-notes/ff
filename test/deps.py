#!/usr/bin/env python3

from subprocess import check_call, check_output, run

core_packages = """
    array
    base-compat
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

cannot_be_avoided = [
    "contravariant",    # `invariant` provides instances for its types
    "distributive",     # needed in `comonad` by `adjunctions`
]

really_used_packages = [
    "crdt",
    "Diff",             # RGA.edit
    "ff",
    "ff-core",
    "ff-qt",
    "github",           # to track github issues
    "gitrev",           # get app version from git
    "inline-c",         # Qt FFI
    "inline-c-cpp",     # Qt FFI
    "ron",
    "ron-rdt",
    "ron-schema",
    "ron-storage",
    "terminal-size",    # for ff CLI
]

questionable_packages = [
    "aeson",    # only string encoding in ron
    "yaml",     # only config
]

dependency_graph = check_output([
    'stack', 'dot',
    '--external',
    '--no-include-base',
    '--prune=' + ','.join(core_packages + cannot_be_avoided),
])

dependency_graph = dependency_graph[0:-2]
for package in really_used_packages:
    dependency_graph += (
        '"{}" [fillcolor = green, style = filled];'.format(package).encode()
    )
for package in questionable_packages:
    dependency_graph += (
        '"{}" [fillcolor = yellow, style = filled];'.format(package).encode()
    )
dependency_graph += b'\n}\n'

# print(dependency_graph)

with open('/tmp/ff.png', 'wb') as f:
    run(['dot', '-Tpng'], check=True, input=dependency_graph, stdout=f)

check_call(['xdg-open', '/tmp/ff.png'])
