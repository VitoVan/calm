set dialogResult to display dialog "This may take a few minutes."  with title "Innitialising CALM..."  buttons {"OK, I will wait", "Why?"}  default button 2 with icon POSIX file "build/calm.icns"  giving up after 60

-- https://en.wikipedia.org/wiki/Meaning_of_life

if button returned of dialogResult is "Why?" then
  set dialogResult to display dialog "You are asking \"the cause, reason, or purpose\", which is a very hard question to answer. \n\nIt's just like the meaning of life, it pertains to the significance of living or existence in general. Many other related questions include: \"Why are we here?\", \"What is life all about?\", or \"What is the purpose of existence?\" There have been many proposed answers to these questions from many different cultural and ideological backgrounds. The search for life's meaning has produced much philosophical, scientific, theological, and metaphysical speculation throughout history. Different people and cultures believe different things for the answer to this question.\n\nIt can be derived from philosophical and religious contemplation of, and scientific inquiries about existence, social ties, consciousness, and happiness. Many other issues are also involved, such as symbolic meaning, ontology, value, purpose, ethics, good and evil, free will, the existence of one or multiple gods, conceptions of God, the soul, and the afterlife.\n\nIf you finished reading the above nonsense copied from Wikipedia, and you still don't have any window showed up, please let me know. There must be something wrong with your CALM installation." buttons {"OK", "Submit an Issue"}
end if

if button returned of dialogResult is "Submit an Issue" then
   open location "https://github.com/VitoVan/calm/issues/new"
end if