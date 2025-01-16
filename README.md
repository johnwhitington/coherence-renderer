Coherence Renderer
==================

Paper:

https://arxiv.org/abs/2411.00131

Abstract:

We describe a hidden surface removal algorithm for two-dimensional layered
scenes built from arbitrary primitives, particularly suited to interaction and
animation in rich scenes (for example, in illustration). The method makes use
of a set-based raster representation to implement a front-to-back rendering
model which analyses and dramatically reduces the amount of rasterization and
composition required to render a scene. The method is extended to add
frame-to-frame coherence analysis and caching for interactive or animated
scenes. A powerful system of primitive-combiners called filters is described,
which preserves the efficiencies of the algorithm in highly complicated scenes.
The set representation is extended to solve the problem of correlated mattes,
leading to an efficient solution for high quality antialiasing. A prototype
implementation has been prepared.

Currently (2025) resurrected to build with modern OCaml and Python, fails on
startup. To fix.
