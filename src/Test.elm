module Test exposing (..)


dummyText =
    """
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut consequat nulla eu elementum pulvinar. Sed venenatis ligula augue, rhoncus maximus erat lobortis nec. Vestibulum viverra ipsum ac mi convallis, nec finibus nisl pulvinar. Sed sodales ex vel erat ornare malesuada. Donec pharetra venenatis nunc vel scelerisque. Duis dolor tortor, efficitur ut nisl ac, venenatis lobortis lacus. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Praesent ut sem eget lectus rutrum tempus eu in tellus. Curabitur rhoncus lacus dui. Mauris tincidunt augue ac nunc facilisis rhoncus et et sapien. Ut tempor, leo ut vehicula faucibus, felis risus molestie neque, a lacinia dolor quam non ex.

Nam tellus arcu, rhoncus id tellus hendrerit, lacinia egestas enim. Vivamus vel quam et sapien pulvinar interdum. Vivamus at gravida eros. Morbi pulvinar, leo sed vestibulum sollicitudin, libero libero mollis orci, id efficitur sem sapien eget mi. Mauris consequat placerat dui, id eleifend eros elementum at. Praesent at viverra purus. Vestibulum molestie nulla eget nisl accumsan viverra. Phasellus consectetur volutpat risus, a pretium felis scelerisque nec. Sed malesuada et est nec mattis. Ut tortor est, sollicitudin elementum congue ut, ornare id leo.

Nunc eget tincidunt nibh. Fusce sit amet libero bibendum, egestas metus ut, accumsan felis. Praesent vitae odio dui. Duis varius risus eget tincidunt faucibus. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Vestibulum quis accumsan libero. Morbi maximus lacus vestibulum euismod convallis. Aliquam aliquam mollis felis, in pharetra lacus porta at. Duis malesuada, augue dapibus imperdiet placerat, lorem tellus tristique erat, ac semper risus felis vel enim. Nunc eget neque lectus. Quisque tempus id purus vitae malesuada. Donec pretium, risus non elementum imperdiet, lectus libero dapibus lorem, sed malesuada orci velit a nulla. Fusce eros ex, rutrum nec imperdiet vel, aliquet in tellus. Nullam blandit, est eu molestie tincidunt, lacus tellus bibendum velit, a ornare sapien leo sit amet metus.

Etiam eget rutrum velit, ac euismod arcu. Aliquam ultricies tortor eu dolor faucibus feugiat. Aliquam placerat eu ante quis tristique. Phasellus suscipit tristique nibh at sodales. Aliquam suscipit, nibh in faucibus convallis, erat tortor imperdiet dui, ut blandit ex felis eu ipsum. Fusce euismod est et ante accumsan, non mattis lacus bibendum. Donec feugiat sem ac nibh egestas lobortis. Nam sed turpis et elit consectetur congue a ac tellus. Nam fringilla metus sed eros efficitur, non consequat neque consequat.

Nulla iaculis a metus ut commodo. Nam ornare sollicitudin leo non tempus. Nulla hendrerit neque turpis, vitae blandit elit finibus in. Proin feugiat mattis ante quis maximus. Aenean in euismod ante. Sed tristique tellus et metus pellentesque gravida. Sed ac laoreet ex, vel faucibus sem. Pellentesque vel tortor rutrum, tincidunt justo in, dictum odio. Vestibulum porta augue ac odio molestie pellentesque. Donec augue quam, ultrices vel tincidunt eu, elementum eu urna. Fusce in purus vehicula, fermentum erat non, vehicula tellus. Phasellus eros tellus, rhoncus lacinia pharetra at, placerat id quam. Mauris eros odio, imperdiet ut nisl eget, finibus egestas mauris. Aliquam erat volutpat.

Donec consectetur aliquam massa sit amet mattis. Donec et nulla nec ipsum scelerisque posuere. Suspendisse rutrum ullamcorper mi. Mauris pharetra a sem laoreet cursus. Suspendisse ac felis luctus, bibendum erat ac, gravida nibh. Curabitur interdum eleifend dolor a egestas. Vivamus sit amet ex posuere metus venenatis scelerisque et nec urna. Praesent at leo fringilla, elementum lorem at, malesuada neque. Nullam blandit, ipsum eu aliquet efficitur, diam sem blandit leo, at vulputate ex nisi vitae ante. Pellentesque ut augue id elit malesuada facilisis at quis tellus. Vestibulum nec augue non turpis hendrerit venenatis sed sed ante. Pellentesque id scelerisque est, vel mollis metus. Mauris sit amet ipsum vitae risus interdum placerat. Aliquam erat volutpat.

Integer nunc turpis, tristique ut leo in, ultricies rutrum purus. Pellentesque sollicitudin tempor est nec dictum. Cras gravida posuere neque eu auctor. Praesent tincidunt arcu id suscipit placerat. Integer in elementum eros. Nullam dictum risus mattis neque euismod, gravida ultricies justo laoreet. Nulla vel tellus in tellus sagittis sollicitudin at non neque. Mauris ipsum ipsum, laoreet in viverra vehicula, rutrum auctor nulla. Mauris at ligula bibendum leo vulputate porta. Pellentesque odio ligula, scelerisque ut ultrices vitae, tempus a elit. Aenean vitae egestas magna, quis aliquet felis.

Nullam fermentum purus vel sapien tristique ullamcorper. Duis at lacus felis. Phasellus eu ex porttitor, lacinia neque et, faucibus ante. Etiam sit amet libero in quam vehicula eleifend at eget dolor. Maecenas semper mi id magna facilisis ultrices. Donec nec enim auctor, aliquet turpis sed, gravida ipsum. Phasellus lacus ex, congue eget urna nec, placerat tincidunt augue. Proin aliquam magna nec ultrices ullamcorper. Proin eleifend urna velit, ullamcorper feugiat felis dignissim sed. Duis risus turpis, mattis a neque vitae, tempus tempus turpis. Donec nec tortor sodales, aliquet leo vitae, lacinia lacus. Vivamus ultricies lacus magna, cursus facilisis erat consequat et. Mauris in facilisis est.
"""


dummyAsciidocText =
    """

== Elm: The Road to Mastery


image::first_image.png[float=right, width=200]

This course is a journey in which you will meet, then
overcome a series of challenges.  Each challenge will
ask that you build something.  At first these will
acts of imitation, for no mastery is possible without
learning from those who have come before.  Later challenges
will ask you to create something of your own invention.


You will make this journey in the company
of your Elm Master, who will help you to find your way.
You may ask your  Master any
questions you have, and ask for whatever help you need.  In the
beginning, you will have many questions and will need much help,
for you are a Novice, and know little about the black
art of Code.  However, as time progresses, you will have fewer questions
will need less help, and steps that were painful and slow
will become effortless and rapid.  How do I know this?  Because, I, your Elm Master,
began as a Novice, as all Masters do.  The path is steep at first, even
frustrating, but soon you gain both confidence and skill, and will
be striding forward with ever quickening pace.  Let the quest begin!

.Master's note
This text, _The Road to Mastery,_ is deliberately spare.
With fewer words to read, there is more time to think, and to do.
Most important, on this journey, you travel with your Master.
He is there for you, and for your questions. _Never
 hesitate to ask a question.  He who asks learns
faster than he who does not._

.A word about mastery
As mentioned, every master was once a Novice, then a Journeyman,
and finally a Master.  But that is not the end.  A Master crafstman
continues to learn from his fellows, and as well, higher masters.
There is always more to learn about one's craft, one's craft is
in a constant state of evolution and change. It is important that
the Novice keep this in mind, for if he becomes a Master and then
stops learning, his power will wither and a master he will no longer be.

== The first mile

Challenge 1:: Look at the web page http://elm-lang.org/examples[Elm examples].
Run some of the code there

Challenge 2::  Use `elm-reactor` to run the program `shapes.elm`. What do you
see when the program runs?  Now look at the code. Study it.  Then explain it
to your Master, line by line.

.Master's note
What is `elm-reactor`?  Where is the program `shapes.elm`?  Who knows?
For that matter, what is code?  If you have not worked with it, touching
it with your bare hands, you do not know, even if you have heard the
words many times. Remember, you are a Novice.  Ask the Master.


Challenge 3:: As you have seen, `shapes.elm` creates an image.  It was a beginner's
image designed for you by your Master.  Make a copy of the file `shapes.elm`.  Then modify it
to make a more interesting picture, one with more shapes.  It should please your
eye.  When it does, look back at the code you have written. It should
also please your eye.  If it does not, edit it.  When you are done,
show the image you have made to your Master.  Also show him your code. Discuss
it with him over a cup of tea.  The master loves conversation, for it reminds
him of his youth.

It is not too early to begin thinking about your own programs, which will be challenges
not set by the Master, but set by you.  Discuss your ideas with your master.
He too has set himself many challenges.

Challenge 4:: You must begin to explore the world on your own. Take a look
at http://elm-lang.org/[elm-lang.org]. And also at this: http://krisajenkins.github.io/elm-rays/[rays].
What do you find?
"""
