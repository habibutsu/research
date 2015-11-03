#! /usr/bin/env python
# encoding: utf-8

from waflib.TaskGen import extension, feature
from waflib import Utils, Task, TaskGen, Context

DEFAULT_CDN = "https://s3.amazonaws.com/s3.hex.pm/tarballs"
DEFAULT_HEX_REGISTRY = "https://s3.amazonaws.com/s3.hex.pm/registry.ets.gz"

def configure(conf):
    conf.find_program('erl', var='ERL')
    conf.find_program('curl')

SCRIPT = """
io:format("Hello world").
"""

class erl(Task.Task):
    always = True
    def run(self):
        cmd = "%s -noshell -eval \"%s\" -s init stop" % (
                    Utils.subst_vars("${ERL}", self.env),
                    self.script.replace('"','\\"').replace("\n", " "))
        print(cmd)
        return self.exec_command(cmd)

@feature('hex_to_index')
def hex_to_index(self):
    self.create_task('erl', None, None, script=SCRIPT)

import waflib

@feature('foo')
def print_hello(self):
    # tgt = self.path.find_or_declare(["registry.ets"])
    # tsk = self.create_task('erl', None, None, script=SCRIPT)
    self.bld(
        rule   = "${ERL} -noshell -eval \"%s\" -s init stop" % SCRIPT.replace('"','\\"'),
        always = True,
        update_outputs = True
    )


@feature('get-deps')
def get_deps(self):

    hex_cache = self.path.find_or_declare(["hex_cache"])
    for pkg in self.pkgs:
        tar_name = "%s-%s.tar" % pkg
        tar_file = hex_cache.find_or_declare([tar_name])
        download_pkg(self, tar_file, pkg)

def download_pkg(self, tar_file, pkg):
    tar_url = DEFAULT_CDN + "/" + tar_file.name
    pkg_dir = tar_file.parent.find_or_declare("%s-%s" % pkg)
    pkg_dir.mkdir()
    pkg_content_dir = pkg_dir.find_or_declare("contents")
    pkg_content_dir.mkdir()
    pkg_content = pkg_dir.find_or_declare("contents.tar.gz")
    metadata_file = pkg_dir.find_or_declare("metadata.config")

    self.bld(
        name="1-%s-%s" % pkg,
        rule="${CURL} %s > %s" % (tar_url, tar_file.abspath()),
        target=tar_file
    )
    self.bld(
        name="2-%s-%s" % pkg,
        rule="tar xf %s -C %s" % (tar_file.abspath(), pkg_dir.abspath()),
        target=pkg_content,
        after="1-%s-%s" % pkg
    )
    self.bld(
        name="3-%s-%s" % pkg,
        rule="tar xf %s -C %s" % (pkg_content.abspath(), pkg_content_dir.abspath()),
        target=pkg_content_dir,
    )

    # tsk = Requirements(env=self.env)
    # tsk.set_inputs(metadata_file)
    # self.bld.add_to_group(tsk)

    tsk = MyLS(env=self.env)
    self.bld.add_to_group(tsk)

class MyLS(Task.Task):
    color = 'PINK'

    def run(self):
        out = self.generator.bld.cmd_and_log("ls -la", quiet=Context.STDOUT)
        #out = Utils.to_list(out)
        print(out)

class Requirements(Task.Task):
    color = 'PINK'

    SCRIPT = """
    {ok, Terms} = file:consult(%(file)s),
    Requirements = proplists:get(<<"requirements">>, Terms),
    io:format("~p~n", Requirements),
    init:stop().
    """

    def run(self):
        script = self.SCRIPT % {"file": self.inputs[0]}
        cmd = "erl -noshell -eval \"%s\"" % script
        out = self.generator.bld.cmd_and_log(cmd , quiet=Context.STDOUT)
        #out = Utils.to_list(out)
        print(out)