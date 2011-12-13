import argparse
import os
import sys

from mutagen.easyid3 import EasyID3
from mutagen.mp3 import MP3
from mutagen.m4a import M4A
from mutagen import File

argparser = argparse.ArgumentParser(
    description="Reorganize MP3 files by tag information")
argparser.add_argument(
    "path", metavar="PATH", nargs=1,
    help="Path to MP3 root directory.")
argparser.add_argument(
    "--nocolor",
    action="store_const",
    const=True,
    default=False,
    help="Disable ANSI terminal colors")

args = argparser.parse_args()

class Colors:
  ESC = "\033["
  PURPLE = ESC + "95m"
  GREEN = ESC + "92m"
  YELLOW = ESC + "93m"
  RESET = ESC + "0m"

class NoColors:
  ESC = ""
  PURPLE = ""
  GREEN = ""
  YELLOW = ""
  RESET = ""

if args.nocolor:
  color_map = NoColors
else:
  color_map = Colors

def log(msg):
  print color_map.GREEN + msg + color_map.RESET

def log_clean(msg):
  print msg

def log_warn(msg):
  print >> sys.stderr, color_map.YELLOW + msg + color_map.RESET

class Stats:
  num_artists = 0
  num_albums = 0
  files_read = 0
  renamed_files = 0
  parse_failed = 0
  type_failed = 0
  missing_tags = 0

  """
  We maintain a map of artist -> album -> title
  """
  file_map = {}

  paths = {
      "parse_failed": [],
      "type_failed": [],
      "missing_tags": []
  }

  @classmethod
  def add_parse_failure(cls, filename):
    cls.paths['parse_failed'].append(filename)
    cls.parse_failed += 1

  @classmethod
  def add_type_failure(cls, filename):
    cls.paths['type_failed'].append(filename)
    cls.type_failed += 1

  @classmethod
  def add_missing_tag_failure(cls, filename, newname):
    cls.paths['missing_tags'].append([filename, newname])
    cls.missing_tags += 1

  @classmethod
  def add_track(cls, artist, album, tracknumber, title):
    files = cls.file_map

    if not files.get(artist):
      files[artist] = {}
      cls.num_artists += 1

    if not files[artist].get(album):
      files[artist][album] = []
      cls.num_albums += 1

    track_list = files[artist][album]
    track_list.append({
      'tracknumber': tracknumber,
      'title': title
    })

    cls.renamed_files += 1

  @classmethod
  def display_stats(cls):
    log("Files read: %d" % (cls.files_read))
    log("Files successfully processed: %d" % (cls.renamed_files))
    log("Parse failures: %d" % (cls.parse_failed))
    for f in cls.paths['parse_failed']:
      log_clean("   %s" % f)
    log("Type determination failures: %d" % (cls.type_failed))
    for f in cls.paths['type_failed']:
      log_clean("   %s" % f)
    log("Files with missing tags: %d" % (cls.missing_tags))
    for f in cls.paths['missing_tags']:
      log_clean("   %s" % f[0])
      log_clean("     %s" % f[1])
    log("Number of artists: %d" % (cls.num_artists))
    log("Number of albums: %d" % (cls.num_albums))

  @classmethod
  def display_map(cls):
    for artist in sorted(cls.file_map):
      log_clean("[ " + artist + " ]")
      for album in sorted(cls.file_map[artist]):
        log_clean(album)
        for track in cls.file_map[artist][album]:
          tracknumber = track['tracknumber'] or "No track"
          title = track['title']
          log_clean("+ %s" % (title))
        print

path = args.path[0]
counter = 0

"""
Recurse down directory path and process each audio file.
"""
for root, dirs, files in os.walk(path):
  for f in files:
    # We maintain a counter to keep file names unique
    counter += 1
    Stats.files_read += 1
    filename = os.path.join(root, f)

    try:
      audio = File(filename, easy=True)
    except Exception, e:
      Stats.add_type_failure(filename)
      continue

    if not audio:
      Stats.add_parse_failure(filename)
      continue

    test_tags = ['artist', 'album', 'title']
    missing_tags = []

    for tag in test_tags:
      if not audio.has_key(tag):
        missing_tags.append(tag)

    tracknumber = audio.get('tracknumber', [None])[0]
    artist = audio.get('artist', ['No Artist'])[0]
    album = audio.get('album', ['No Album'])[0]
    title = audio.get('title', ["%d_%s" % (counter, f)])[0]

    if not tracknumber:
      newname = os.path.join(artist, album, title)
    else:
      newname = os.path.join(artist, album, "%s - %s" % (tracknumber, title))

    if len(missing_tags) > 0:
      pretty_missing_tags = str.join(", ", missing_tags)
      Stats.add_missing_tag_failure(
          filename + ": " + pretty_missing_tags,
          newname)

    Stats.add_track(artist, album, tracknumber, title)

Stats.display_map()
print
Stats.display_stats()


