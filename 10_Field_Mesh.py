import bpy
import csv
import bmesh

bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete()

### field base
file_loc = r"C:\Users\Sully\OneDrive\Documents\SMT26\ANI_field_mesh.csv"

bm = bmesh.new()
points = {}
min_x = 0
max_x = 0
min_y = 0
max_y = 0

with open(file_loc, newline='') as f:
    locations = csv.DictReader(f)
  
    for row in locations:
      x = int(float(row['ball_position_x']))
      y = int(float(row['ball_position_y']))
      z = float(row['ball_position_z'])
      points[(x, y)] = bm.verts.new((x, y, z))
      min_x = min(min_x, x)
      max_x = max(max_x, x)
      min_y = min(min_y, y)
      max_y = max(max_y, y)


bm.verts.ensure_lookup_table()

for x in range(min_x, max_x):
    for y in range(min_y, max_y):

        point_1 = points.get((x, y))
        point_2 = points.get((x + 1, y))
        point_3 = points.get((x + 1, y + 1))
        point_4 = points.get((x, y + 1))

        vertices = [point_1, point_2, point_3, point_4]
        vertices = [vert for vert in vertices if vert is not None]
        if len(vertices) >= 3:
            bm.faces.new(vertices)


field = bpy.data.meshes.new("GridSurface")
bm.to_mesh(field)
bm.free()

obj = bpy.data.objects.new("GridSurface", field)
bpy.context.collection.objects.link(obj)


mat = bpy.data.materials.new("grass")
mat.node_tree.nodes["Principled BSDF"].inputs["Base Color"].default_value = (0, 1, 0, 1)
obj.data.materials.append(mat)


######################################################################################################################################################################################

import bpy
import csv
import bmesh

bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete()

### foul lines
file_loc = r"C:\Users\Sully\OneDrive\Documents\SMT26\ANI_foul_lines.csv"

bm = bmesh.new()
points = {}
min_x = 0
max_x = 0

with open(file_loc, newline='') as f:
    locations = csv.DictReader(f)
  
    for row in locations:
      x = int(float(row['ball_position_x']))
      y = float(row['ball_position_y'])
      z = float(row['ball_position_z'])
      points[(x, y)] = bm.verts.new((x, y, z))
      min_x = min(min_x, x)
      max_x = max(max_x, x)


bm.verts.ensure_lookup_table()

for x in range(min_x, max_x):
    point_1 = points.get((x, abs(x)))
    point_2 = points.get((x, abs(x) + 0.4714))
    point_3 = points.get((x + 1, abs(x) - 1 + 0.4714))
    point_4 = points.get((x + 1, abs(x) + 1 + 0.4714))
    point_5 = points.get((x + 1, abs(x) - 1))
    point_6 = points.get((x + 1, abs(x) + 1))

    vertices = [point_1, point_2, point_3, point_4, point_5, point_6]
    vertices = [vert for vert in vertices if vert is not None]
    if len(vertices) == 4:
        bm.faces.new(vertices)


field = bpy.data.meshes.new("GridSurface")
bm.to_mesh(field)
bm.free()

obj = bpy.data.objects.new("GridSurface", field)
bpy.context.collection.objects.link(obj)


mat = bpy.data.materials.new("foul_line")
mat.node_tree.nodes["Principled BSDF"].inputs["Base Color"].default_value = (1, 1, 1, 1)
obj.data.materials.append(mat)

