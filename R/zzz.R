#' @export
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(sample(c("'Essentially, all models are wrong, but some are useful' - George E.P. Box, British statistician",
                                 "'You can’t use up creativity. The more you use, the more you have.' - Mary Angelou, American poet, singer, memoirist, and civil rights activist",
                                 "'I am the walrus' - Lennon-McCartney",
                                 "'A trend is a trend is a trend. But the question is, will it bend? Will it alter its course through some unforeseen force and come to a premature end?' - Alexander Cairncross, British Economist",
                                 "'The Analytical Engine has no pretensions whatever to originate anything. It can do whatever we know how to order it to perform.' - Ada Lovelace, English mathematician and writer",
                                 "'Draw your assumptions before your conclusions' - Miguel Hernan, Spanish-American physician and epidemiologist",
                                 "'Statistics is the grammer of Science' - Karl Pearson, English statistician",
                                 "'Science and everyday life cannot and should not be seperated' - Rosalind Franklin, English chemist ",
                                 "'There are three kinds of lies - lies, damned lies and statistics' - Mark Twain, American writer",
                                 "'Everybody might be just one big soul, well it looks that a-way to me' - Woody Guthrie, American folk singer",
                                 "'He uses statistics as a drunken man uses lamp posts – for support rather than for illumination.' - Andrew Lang, Scottish writer",
                                 "'Know how to learn. Then, want to learn' - Katherine Johnson, American mathematician and NASA space scientist",
                                 "'I was taught that the way of progress was neither swift nor easy.' - Marie Curie, French-Polish physicist",
                                 "'There's no use boiling your cabbage twice' - Irish proverb",
                                 "'It's the inspired student that continues to learn on their own. That's what separates the real achievers in the world from those who pedal along, finishing assignments.' - Neil deGrasse Tyson, American astrophysicist, author, and science communicator",
                                 "'If you start wielding a hammer, then all your problems look like nails. And maybe they’re not.' - Neil deGrasse Tyson, American astrophysicist, author, and science communicator",
                                 "'Not everything that can be counted counts, and not everything that counts can be counted.' - Albert Einstein, German-American theoretical physicist",
                                 "'I don’t know that I’ll be drinking a beer tonight, I might go a little higher up the shelf.' - The Hon. Daniel Andrews, Premier of Victoria (On announcing relaxed restrictions following the extended COVID-19 lockdown in Victoria throughout winter 2020)"
  )
  , 1)
  )
}
