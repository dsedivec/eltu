#!/usr/bin/env python2
#
# Currently requires Python 2.6 because of the use of the json module.
# Also using subprocess.check_call which is 2.5.

import sys
import os
import os.path
import re
import subprocess
import json
import shutil
import logging


def main(argv):
    # Get temp_tags_file into a variable and start this try/except as
    # early as possible, because it's our responsibility to rm this
    # temp file if things blow up.
    temp_tags_file = argv[2]
    try:
        ctags_command = json.loads(argv[1])
        tags_file = argv[3]
        tags_file_dir = os.path.abspath(os.path.dirname(tags_file))
        files_to_update = argv[4:]
        file_regexps = []
        for file_name in files_to_update:
            if not os.path.abspath(file_name):
                raise Exception(
                    "all supplied paths must be absolute, but %r is not"
                    % (file_name,))
            file_regexps.append(re.escape(file_name))
            relative_file_name = os.path.relpath(file_name, tags_file_dir)
            file_regexps.append(r"(?:\./)?%s"
                                % (re.escape(relative_file_name),))
        file_names_regexp = re.compile(r"^(?:%s)$" % ("|".join(file_regexps),))
        logging.debug("file_names_regexp: %s", file_names_regexp.pattern)
        with open(tags_file, "rb") as old_tags_file, \
             open(temp_tags_file, "wb") as new_tags_file:
            while True:
                form_feed = old_tags_file.readline()
                if not form_feed:
                    # EOF
                    break
                elif form_feed != "\f\n":
                    raise Exception(("expected form feed in tags file,"
                                     " instead got %r near %d")
                                    % (form_feed, old_tags_file.tell()))
                file_line = old_tags_file.readline()
                try:
                    file_name, tags_size = file_line.rstrip().rsplit(r",", 1)
                except Exception, ex:
                    raise Exception(
                        "couldn't parse tags file line %r near %d: %s: %s"
                        % (file_line, old_tags_file.tell(),
                           ex.__class__.__name__, ex))
                logging.debug("found file name: %s", file_name)
                if not file_names_regexp.search(file_name):
                    logging.debug("copying tags for that file")
                    new_tags_file.write(form_feed)
                    new_tags_file.write(file_line)
                    new_tags_file.write(old_tags_file.read(int(tags_size)))
                else:
                    logging.debug("ignoring tags for that file")
                    old_tags_file.seek(int(tags_size), os.SEEK_CUR)
        logging.debug("ctags_command=%r", ctags_command)
        subprocess.check_call(ctags_command)
        logging.debug("replacing %r with %r", tags_file, new_tags_file.name)
        shutil.move(temp_tags_file, tags_file)
    except Exception:
        logging.exception("got exception")
        try:
            os.unlink(temp_tags_file)
        except Exception:
            pass
        raise


if __name__ == "__main__":
    main(sys.argv)
