# Keeps a perforce depot & a git branch in sync

def run_cmd str
  puts str
  result = `#{str}`
  result.each_line {|line| puts line}
end

GIT_PERFORCE_BRANCH = 'perforce'

# Make sure we are on the proper git branch
run_cmd "git checkout #{GIT_PERFORCE_BRANCH}"

# NOTE: it is important to have your perforce client spec set up to clobber files.
# Otherwise you will get lots of cant clobber errors.

# sync
run_cmd "p4 sync"

# add any new files
# TODO:

run_cmd "git commit -a -m \"p4 sync\""
