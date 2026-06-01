import bpy
import csv

bpy.ops.object.select_all(action='SELECT')
bpy.ops.object.delete()


bpy.ops.mesh.primitive_uv_sphere_add(radius=1, enter_editmode=False, align='WORLD', location=(0, 0, 0), scale=(1, 1, 1))
baseball = bpy.context.active_object

file_loc = r"C:\Users\Sully\OneDrive\Documents\SMT26\ex_ball_flight.csv"

bpy.context.scene.render.fps = 60
fps = 60

with open(file_loc, newline='') as f:
    locations = csv.DictReader(f)
  
    for row in locations:

      baseball.location = (
          float(row['ball_position_x']),
          float(row['ball_position_y']),
          float(row['ball_position_z'])
      )
  
      baseball.keyframe_insert(
          data_path="location",
          frame = 1 + round( ((float(row['timestamp'])/1000)*fps) )
      )
  

for fcurve in baseball.animation_data.action.fcurves:
    for keyframe in fcurve.keyframe_points:
        keyframe.interpolation = 'LINEAR'
