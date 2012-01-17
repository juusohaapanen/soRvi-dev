#!/usr/bin/python
# coding=utf-8

from optparse import OptionParser
import os
import shutil
import sys

# Install GitPython (tested with 0.3.2.RC1) 
#   > easy_install GitPython
# or
#   > pip install GitPython 
# or
#   download from http://pypi.python.org/pypi/GitPython

try:
    from git import InvalidGitRepositoryError, Repo
except ImportError:
    print("ERROR: Could not import package GitPython")
    sys.exit(1)

def main():
    '''Hack magic to mirror the changes of the last git commit to another 
    folder. 
    '''
    
    usage = "Usage: %prog [-ls] fromdir [targetdir]"
    
    parser = OptionParser(usage)
    
    parser.add_option("-l", "--list", default=False,
                      action="store_true", dest="_list",
                      help="List the files affected by the last git commit")
    
    parser.add_option("-s", "--safe", default=False,
                      action="store_true", dest="safe",
                      help="Safe mode: ask for confirmation for all " + \
                           "copy/replace/delete operations")
    
    (options, args) = parser.parse_args()
    
    # Source dir must be provided under all circumstances
    if len(args) < 1:
        print("ERROR: Source directory must be provided.")
        return 1
    else:
        # Source dir should also be an existing directory
        fromdir = os.path.abspath(args[0])
        if os.path.exists(fromdir):
            print("Source dir: %s" % fromdir)
        else:
            print("ERROR: Source dir %s does not exist." % fromdir)
            return 1
        try:
            # Finally source directory must also host a git repo
            repo = Repo(fromdir)
        except InvalidGitRepositoryError:
            print("ERROR: Source dir is not a git repository.")
            return 1

    # Get the informarion from the last git commit as String and split 
    # by newlines
    last_commit = repo.git.whatchanged(-1, oneline=True).split("\n")
    # Further process each line by splitting with whitespace
    last_commit = [item.split(" ") for item in last_commit]
    # Split each line component with tab, only the last component should have
    # tab as a separator (e.g. "M\tdir/file")    
    last_commit = [[subitem.split("\t") for subitem in item] for item in last_commit]
    # Create an empty dictionary -> 'A'(dded), 'M'(odified) and 'D'(eleted)
    # will be the keys, relative filepaths (relative to the git root) the 
    # values     
    changed_files = {}    
    for tag in ['A', 'M', 'D']:
        # Initialize each key (commit category) with an empty list         
        changed_files[tag] = []
    
    # Assign each file into a corresponding commit category       
    for item in last_commit:
        # The first item in the commit set is the commit message, which only
        # has 1 item
        if len(item[-1]) > 1:
            # Relative file name
            _file = item[-1][1]
            # Commit tag ('A', 'M' or 'D')
            tag = item[-1][0]
            changed_files[tag].append(_file)
    
    # List all the files and commit category tags if requested
    if options._list:
        print("Content of the last commit")
        if changed_files['A']:
            print('Added files:')
            for _file in changed_files['A']:
                print('\t%s' % _file)
        if changed_files['M']:
            print('Modified files:')
            for _file in changed_files['M']:
                print('\t%s' % _file)
        if changed_files['D']:
            print('Deleted files:')
            for _file in changed_files['D']:
                print('\t%s' % _file)
        
    # Assume that if no target directory is provided user only wants to list
    # (print) files
    if options._list and len(args) == 1:
        return 0
    
    # If user did not request for listing and did not provide a target 
    # directory, give an error
    if len(args) < 2:
        print("ERROR: Target directory must be provided.")
        return 1
    else:
        targetdir = os.path.abspath(args[1])
    
    # Target directory must exist. NOTE: other than checking whether the target
    # (root) directory exists, the script makes no further checks whether the
    # folder structure really mirrors that of the source dir. The relative
    # path *within* the target directory comes from the relative path in the 
    # source directory (as given by git's commit information). If the target
    # directory does not have a similar folder structure an error will be 
    # raised when trying to copy/delete the file in the target directory.
    if os.path.exists(targetdir):
        print("Target directory: %s" % targetdir)
    else:
        print("ERROR: Target dir %s does not exist." % targetdir)
        return 1
    
    for _file in changed_files['A']:
        # Adding means there is no counterpart in the target directory so
        # we will just copy the file over to the target directory
        source_file = os.path.join(fromdir, _file)
        target_file = os.path.join(targetdir, _file)
        if options.safe:
            go = raw_input("Copy file %s? [Y/n]" % source_file)
        else:
            go = 'Y'
        if go not in ['n', 'N', 'no', 'NO']:
            print("Copying %s -> %s" % (source_file, target_file))
            try:
                shutil.copy2(source_file, target_file)
            except IOError, e:
                print("ERROR: Could not copy file %s" % e)
    
    for _file in changed_files['M']:
        # Modifiying means that there should be a counterpart in the target 
        # directory. We will try to overwrite the existing file.
        source_file = os.path.join(fromdir, _file)
        target_file = os.path.join(targetdir, _file)
        # See if the target file truly exists, if it does not, copy the
        # the source file regardless
        if os.path.exists(target_file):
            if options.safe:
                go = raw_input("Overwrite file %s? [Y/n]" % target_file)
            else:
                go = 'Y'
            if go not in ['n', 'N', 'no', 'NO']:
                print("Overwriting %s -> %s" % (source_file, target_file))        
                try:            
                    shutil.copy2(source_file, target_file)
                except IOError, e:
                    print("ERROR: Could not overwrite file %s" % e)
        else:
            print("Target file %s not found" % target_file)
            if options.safe:
                go = raw_input("Copy file %s? [Y/n]" % source_file)
            else:
                go = 'Y'
            if go not in ['n', 'N', 'no', 'NO']:
                print("Copying %s -> %s" % (source_file, target_file))
                try:
                    shutil.copy2(source_file, target_file)
                except IOError, e:
                    print("ERROR: Could not copy file %s" % e)
    
    for _file in changed_files['D']:
        # Deleting means that there should be a counterpart in the target 
        # directory. We will try to delete the existing file.        
        target_file = os.path.join(targetdir, _file)
        if os.path.exists(target_file):
            if options.safe:
                go = raw_input("Delete file %s? [Y/n]" % target_file)
            else:
                go = 'Y'
            if go not in ['n', 'N', 'no', 'NO']:
                print("Deleting %s" % target_file)        
                try:            
                    os.remove(target_file)
                except OSError, e:
                    print("ERROR: Could not delete file %s" % e)
        else:
            print("File %s not found" % target_file)
    
    print "\nCopying finished."
    return 0
    
if __name__ == "__main__":
    sys.exit(main()) 
    