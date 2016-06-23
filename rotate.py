#!/usr/bin/python

import pygame
from pygame.locals import *
import thread
import random
import math

def rot_center(image, angle):
    """rotate an image while keeping its center and size"""
    orig_rect = image.get_rect()
    rot_image = pygame.transform.rotate(image, angle)
    rot_rect = orig_rect.copy()
    rot_rect.center = rot_image.get_rect().center
    rot_image = rot_image.subsurface(rot_rect).copy()
    return rot_image

width = 1920
height = 1200
radius = 2264
SIZE = (width, height)
pygame.init ()
#screen = pygame.display.set_mode (SIZE, pygame.FULLSCREEN)
screen = pygame.display.set_mode (SIZE)

screen.fill ((0, 0, 0))
dots = pygame.image.load ("dots.png").convert_alpha ()
circle = pygame.image.load ("circle.png").convert_alpha ()
table = pygame.image.load ("table.png").convert_alpha ()
obj = [pygame.image.load ("obj-lo.png").convert_alpha (),
       pygame.image.load ("obj-mi.png").convert_alpha (),
       pygame.image.load ("obj-hi.png").convert_alpha ()]
csize = 292

angle = 0
done = False
speed = 30.
frate = 30.
tsamp = int (round (1000. / frate))
dangle = speed / frate

dots_rot = [None] * 360
clock_radius = 100
for i in range (360):
    dots_rot [i] = rot_center (dots, -i)
    pygame.draw.arc (screen, (0, 0, 255),
                     (width / 2 - clock_radius + 3, height / 2 - clock_radius + 3,
                      2 * clock_radius - 6, 2 * clock_radius - 6),
                     0, 2 * math.pi * i, 4)
    pygame.draw.arc (screen, (255, 255, 0),
                     (width / 2 - clock_radius, height / 2 - clock_radius,
                      2 * clock_radius, 2 * clock_radius),
                     0, math.pi * i / 180, 10)
    pygame.display.flip ()

bg_angle = 0
show = False
obj_rot = None

def show_bg ():
    global done
    global bg_angle
    while not done:
        screen.blit (dots_rot [bg_angle], (-(radius - width) / 2, -(radius - height) / 2))
        screen.blit (circle, ((width - csize) / 2, (height - csize) / 2))
        bg_angle += 1
        if bg_angle == 360:
            bg_angle = 0
        if show:
            screen.blit (table, ((width - csize) / 2, (height - csize) / 2))
            screen.blit (obj_rot, ((width - csize) / 2, (height - csize) / 2))
        pygame.display.flip ()
        pygame.time.wait (tsamp)
        for e in pygame.event.get():
            if e.type == QUIT or (e.type == KEYDOWN and e.key == K_ESCAPE):
                done = True
                break

print ("count,object,angle")

def show_stim ():
    global obj_rot
    global show
    count = 1
    pygame.time.wait (60000)
    while not done:
        show = False
        pygame.time.wait (2000)
        a = 10 + random.random () * 30
        i = random.randint (0, 2)
        print ("%d,%d,%f" % (count,i,a))
        count += 1
        obj_rot = rot_center (obj [i], a)
        show = True
        pygame.time.wait (500)

thread.start_new_thread (show_bg, ())
thread.start_new_thread (show_stim, ())

while not done:
    pygame.time.wait (1000)

pygame.time.wait (2000)

