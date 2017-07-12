# @example
#
# rake hlint:install[2.0.9]

namespace :hlint do

  desc "Download and install hlint"
  task :install, :version do |task, args|
    REPO = "https://github.com/ndmitchell/hlint"
    VERSION = args[:version].to_s
    ASSET = "hlint-#{VERSION}-x86_64-linux.tar.gz"
    URL = "#{REPO}/releases/download/v#{VERSION}/#{ASSET}"
    DESTINATION_BASE = File.expand_path(File.join(File.dirname(__FILE__), 'bin'))
    DESTINATION_TMP = File.join("#{DESTINATION_BASE}", 'tmp')

    puts "Downloading hlint@v#{VERSION}"
    sh [
      "mkdir -p #{DESTINATION_TMP}",
      "curl -s -L #{URL} -o #{ASSET}",
      "tar -xf #{ASSET} -C #{DESTINATION_TMP}",
      "cp #{DESTINATION_TMP}/hlint-#{VERSION}/hlint #{File.expand_path("~/.local/bin")}",
      "cp -R #{DESTINATION_TMP}/hlint-#{VERSION}/data #{File.expand_path("~/.local/bin")}",
      "rm -r #{DESTINATION_BASE}/",
      "rm #{ASSET}"
    ].join(" && ")
  end

end

