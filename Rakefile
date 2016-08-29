require 'ebook_library'
task :generate do
  default = EbookLibrary.default_path
  path = ARGV[1] || default
  if File.directory?(path)
    ebooks = EbookLibrary.generate :json
    File.open("./books.json", 'w') { |file| file.write(ebooks) }
  else
    raise InvalidPath, "#{path} is an invalid path for gathering"
  end
end

class InvalidPath < StandardError; end
