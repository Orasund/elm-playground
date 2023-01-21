module Chapter exposing (..)

import ElmBook.Chapter exposing (Chapter)


one : Chapter state
one =
    ElmBook.Chapter.chapter "Introduction to Hyperbolic space"
        |> ElmBook.Chapter.render """
The famous greek mathematician Euclid was the first person to write down a set of rules which with one could reason about geometry.

In his [book series "Elements"](https://en.wikipedia.org/wiki/Euclid%27s_Elements) he wrote down five statements, that we have to assume to be true, in order to do geometry.

The first four statements where fundamental things about geometry, but the last one stated that parallel lines never meet:

> If a line segment intersects two straight lines forming two interior angles on the same side that sum to less than two right angles, then the two lines, if extended indefinitely, meet on that side on which the angles sum to less than two right angles. _- Book 1 of the Elements by Euclid_

Through out history mathematicians had the feeling that this sentence does not need to be assumed, but instead that it could be actually proven. And so again and again they tried and failed. Until in 1829 the mathematician [Nikolai Lobachevsky](https://en.wikipedia.org/wiki/Nikolai_Lobachevsky) noticed that you can actually assume this statement to be false and still do geometry.

[![Image showing the difference between euclidean, hyperbolic and spherical geometry](https://upload.wikimedia.org/wikipedia/commons/thumb/7/78/Noneuclid.svg/1920px-Noneuclid.svg.png)](https://en.wikipedia.org/wiki/Euclid%27s_Elements#/media/File:Noneuclid.svg)
_parallel lines in hyperbolic-, euclidean- and elliptic(spherical)-geometry._

One obvious way how the statement would be wrong is if two line segments **always** meet.

This is in fact the case for [spherical geometry](https://en.wikipedia.org/wiki/Spherical_geometry) on a ball (like the earth).

[![Image of two straight lines on earth meeting at the north pole](https://upload.wikimedia.org/wikipedia/commons/thumb/9/97/Triangles_%28spherical_geometry%29.jpg/800px-Triangles_%28spherical_geometry%29.jpg)](https://en.wikipedia.org/wiki/Spherical_geometry#/media/File:Triangles_%28spherical_geometry%29.jpg)
_On earth two straight lines with 90° can still meet_

This is also true for projective geometry (like photos of the real world).

![Train tracks that seemingly meet at the horizon.](https://www.publicdomainpictures.net/pictures/30000/velka/train-tracks-1343121290P8s.jpg)
_You can point to the spot on the image where the train tracks seemingly meet on the horizon._

But Lobachevsky had a different kind of geometry in mind. One where parallel lines move further and further apart as they go to infinity. 
It's hard to grasp how such a space would look like and we can only ever see projections of such a space into the euclidean world (our world).

This geometry is called hyperbolic geometry. In this book we want to investigate this weird world and get a feeling for all its oddities.

[![Limit III by M.C. Escher ](https://upload.wikimedia.org/wikipedia/en/5/55/Escher_Circle_Limit_III.jpg)](https://en.wikipedia.org/wiki/Circle_Limit_III#/media/File:Escher_Circle_Limit_III.jpg)
_Limit III by M.C. Escher is a look into the world of hyperbolic geometry_

If you want to get a better understanding of hyperbolic geometry, then you can [check out this video](https://www.youtube.com/watch?v=zQo_S3yNa2w) by the creator of [Hyperbolica](https://store.steampowered.com/app/1256230/Hyperbolica/), a First-Person Hyperbolic computer game.
"""
