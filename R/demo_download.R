options(timeout=4000)
download.file("https://www.dropbox.com/scl/fi/z57rmeg013lxktpzdkb98/Archive.zip?rlkey=uo6ypt8g02bjqlq287cqecqsc&dl=1", destfile = paste0(tempdir(),"demo_tiffs.zip"))


unzip(paste0(tempdir(),"demo_tiffs.zip"),exdir = "../data/Tiffs")

unlink("../data/Tiffs/__MACOSX/",recursive =T)
