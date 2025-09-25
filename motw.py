import csv
import graphviz
import uuid

import os
os.environ["PATH"] += os.pathsep + 'C:/Program Files/Graphviz/bin/'

CURRENT_YEAR = 25
CURRENT_WEEK = 4
CURRENT_CHAMPION = "D.K. Lost a Minion"
matchups = [['Week', 'Champion', 'Opponent']]


class Node:
    def __init__(self, value):
        self.left = None
        self.right = None
        self.label = value


def build(name, week, last_week, schedule, dot, my_uuid):
    if week == last_week:
        dot.node(my_uuid, name + " (Week " + str(week) + ")")
        return name
    else:
        left_uuid = str(uuid.uuid1())
        right_uuid = str(uuid.uuid1())

        # visualize
        dot.node(my_uuid, name + " (Week " + str(week) + ")")
        dot.edge(my_uuid, left_uuid)
        dot.edge(my_uuid, right_uuid)

        # create lists of matchups
        matchups.append([week+1, name, schedule[week+1][name]])

        # build data structure
        node = Node(name)
        node.left = build(name, week+1, last_week, schedule, dot, left_uuid)
        node.right = build(schedule[week+1][name],
                           week+1, last_week, schedule, dot, right_uuid)
        return node, dot


def make_schedule_map():
    schedule = dict()
    with open('currentMotW/schedule25.csv', newline='') as csvfile:
        reader = csv.reader(csvfile, delimiter=',')
        for row in reader:
            week = int(row[0])
            opp1 = row[1].strip()
            opp2 = row[2].strip()

            if week not in schedule:
                schedule[week] = dict()
            schedule[week][opp1] = opp2
            schedule[week][opp2] = opp1
    return schedule


def main():
    schedule = make_schedule_map()

    # full future
    dot = graphviz.Digraph(comment='Matchup of the Week')

    tree, dot = build(CURRENT_CHAMPION, CURRENT_WEEK-1,
                      14, schedule, dot, str(uuid.uuid1()))

    dot.render("currentMotW/MotW_Future")

    with open('currentMotW/matchup_tree.csv', 'w') as myfile:
        wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
        wr.writerows(matchups)

    # near future png for newspaper
    dot = graphviz.Digraph(comment='Matchup of the Week',
                           graph_attr={'rankdir': 'LR'})

    tree, dot = build(CURRENT_CHAMPION, CURRENT_WEEK-1,
                      CURRENT_WEEK + 2, schedule, dot, str(uuid.uuid1()))

    # Use raw string for the file path to avoid issues with backslashes
    path = os.path.join("..", "theoffensiveline-site", "src", "newsletters", 
                    f"20{CURRENT_YEAR}", f"20{CURRENT_YEAR} Week {CURRENT_WEEK-1}", "motwFuture")
    print(path)
    dot.render(path, format='png', cleanup=True)
    
    del matchups[1:]
    
    # left tree

    tree, dot = build(CURRENT_CHAMPION, CURRENT_WEEK, 14,
                      schedule, dot, str(uuid.uuid1()))

    with open('currentMotW/matchup_tree_left.csv', 'w') as myfile:
        wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
        wr.writerows(matchups)

    del matchups[1:]

    # right tree

    tree, dot = build(schedule[CURRENT_WEEK][CURRENT_CHAMPION],
                      CURRENT_WEEK, 14, schedule, dot, str(uuid.uuid1()))

    with open('currentMotW/matchup_tree_right.csv', 'w') as myfile:
        wr = csv.writer(myfile, quoting=csv.QUOTE_ALL)
        wr.writerows(matchups)


main()
