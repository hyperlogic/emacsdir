
# Builds emacs TAGS files from all the src files in the PATHS directory.

PATHS = ['~/code/hifi/libraries',
         '~/code/hifi/interface/src']

# ends in .h, .cpp or .c (case insensitive match)
SRC_PATTERN = /\.[hH]\z|\.[cC][pP][pP]\z|\.[cC]\z/

OUTPUT_FILE = 'srcfiles.txt'

class Dir

  # for each file in path, including sub-directories, 
  # yield the full path of the filename to a block
  def Dir.for_each_rec path, &block
    exp_path = File.expand_path(path)
    if File.directory?(exp_path)
      foreach exp_path do |file|
        if file[0,1] != '.'
          for_each_rec File.expand_path(file, exp_path), &block
        end
      end
    else
      yield path
    end
  end
end

# write out a file with all the source files in PATHS
File.open(OUTPUT_FILE, 'wb') do |out|
  PATHS.each do |path|
    Dir.for_each_rec(path) do |f| 
      if SRC_PATTERN =~ f
        out.puts f
      end
    end
  end
end

# shell out to ebrowse to build the emacs BROWSE file
`etags --language=c++ - < #{OUTPUT_FILE}`
