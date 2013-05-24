

Maid.rules do
  def date_folder(p)
    return File::atime(p).strftime("%Y-%m-%d")
  end

  rule 'Archive downloads' do
    (1..10).each do |n|
      # Trash duplicate downloads
      trash(dir("~/Downloads/* (#{n}).*"))
    end
    dir('~/Downloads/*').each do |p|
      # Move to date folder
      if 24.hours.since?(accessed_at(p)) and not p.end_with?("Archive")
        adir = "~/Downloads/Archive/#{date_folder(p)}"
        mkdir(adir)
        move(p, adir)
      end
    end
  end
end
