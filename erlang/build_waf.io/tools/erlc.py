#! /usr/bin/env python
# encoding: utf-8

from waflib.TaskGen import extension, feature
from waflib import Utils, Task, TaskGen

DEFAULT_CDN = "https://s3.amazonaws.com/s3.hex.pm/tarballs"

class erlc(Task.Task):
    run_str='${ERLC} -o ${TGT[0].parent.relpath()} ${SRC}'

class cp(Task.Task):
    run_str='${CP} ${SRC} ${TGT}'

# class curl(Task.Task):
#     update_outputs = True

#     def run(self):
#         cmd = "%s %s > %s" % (Utils.subst_vars("${CURL}", self.env), self.url, self.outputs[0].relpath())
#         return self.exec_command(cmd)

def configure(conf):
    conf.find_program('erlc', var='ERLC')
    conf.find_program('tar')
    conf.find_program('cp')
    #conf.find_program('curl')


@extension('.erl')
def add_erl(self, node):
    beam = node.change_ext('.beam')
    tgt = beam.parent.parent.find_or_declare(["ebin", beam.name])
    tsk = self.create_task('erlc', src=node, tgt=tgt)
    return tsk

@extension('.app.src')
def add_app_src(self, node):
    beam = node.change_ext('')
    tgt = beam.parent.parent.find_or_declare(["ebin", beam.name])
    tsk = self.create_task('cp', src=node, tgt=tgt)
    return tsk


# @feature('get-deps')
# def get_deps(self):
#     libs = self.path.find_or_declare(["libs"])
#     for pkg in self.pkgs:
#         tar = "%s-%s.tar" % pkg
#         tar_url = DEFAULT_CDN + "/" + tar
#         tgt = libs.find_or_declare([tar])
#         tsk = self.create_task('curl', None, tgt, url=tar_url)
