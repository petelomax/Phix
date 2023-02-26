-- demo\rosetta\mlijobs.e
constant raw_text = """
License OUT @ 2008/10/02_23:31:01 for job 0
License OUT @ 2008/10/02_23:36:30 for job 1
License OUT @ 2008/10/02_23:38:18 for job 2
License OUT @ 2008/10/02_23:38:39 for job 3
License IN  @ 2008/10/03_00:00:35 for job 0
License OUT @ 2008/10/02_23:39:39 for job 4
License OUT @ 2008/10/02_23:46:24 for job 5
License OUT @ 2008/10/02_23:48:13 for job 6
License OUT @ 2008/10/02_23:48:13 for job 7
License IN  @ 2008/10/03_00:02:22 for job 1
License OUT @ 2008/10/02_23:48:52 for job 8
License IN  @ 2008/10/03_00:09:34 for job 6
License OUT @ 2008/10/02_23:49:02 for job 9
License IN  @ 2008/10/03_00:02:34 for job 4
License IN  @ 2008/10/03_00:06:26 for job 3
License IN  @ 2008/10/03_00:08:03 for job 2
License IN  @ 2008/10/03_00:09:12 for job 5
License IN  @ 2008/10/03_00:11:52 for job 8
License IN  @ 2008/10/03_00:12:51 for job 7
License IN  @ 2008/10/03_00:17:19 for job 9
"""
global constant lines = split(raw_text,"\n")
