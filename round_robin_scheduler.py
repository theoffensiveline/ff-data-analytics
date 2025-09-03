#!/usr/bin/env python3
"""
Round Robin Tournament Scheduler

This script generates a random schedule where each person plays against every other person exactly once.
"""

import random
import sys
import csv
from typing import List, Tuple


def create_round_robin_schedule(names: List[str]) -> List[List[Tuple[str, str]]]:
    """
    Create a round-robin tournament schedule where each person plays against every other person exactly once.
    The rounds are shuffled to eliminate any pattern.
    
    Args:
        names: List of participant names
        
    Returns:
        A list of rounds, where each round is a list of matches
    """
    # Make sure we have an even number of participants
    if len(names) % 2 != 0:
        names = names + ["BYE"]  # Add a bye if odd number of participants
    
    n = len(names)
    
    # Create a copy of names to shuffle
    players = names.copy()
    random.shuffle(players)
    
    # Standard algorithm for round robin tournament
    # Keep one player fixed and rotate the rest
    rounds = []
    
    for i in range(n - 1):
        round_matches = []
        for j in range(n // 2):
            # Match players[j] with players[n-1-j]
            # This ensures everyone plays against everyone else exactly once
            player1 = players[j]
            player2 = players[n - 1 - j]
            
            # Don't include matches with "BYE"
            if player1 != "BYE" and player2 != "BYE":
                round_matches.append((player1, player2))
        
        rounds.append(round_matches)
        
        # Rotate players: keep players[0] fixed, move players[1:] one position
        players = [players[0]] + [players[-1]] + players[1:-1]
    
    # Shuffle the rounds to eliminate any pattern
    random.shuffle(rounds)
    
    return rounds


def print_schedule(rounds: List[List[Tuple[str, str]]]) -> None:
    """
    Print the schedule in a readable format.
    
    Args:
        rounds: List of rounds, where each round is a list of matches
    """
    print("\nTournament Schedule:")
    print("===================\n")
    
    # Print regular round-robin rounds (first 11 rounds)
    for i, round_matches in enumerate(rounds[:11], 1):
        print(f"Week {i} (Regular):")
        for match in round_matches:
            print(f"  {match[0]} vs {match[1]}")
        print()
    
    # Print division matchups rounds (last 3 rounds)
    for i, round_matches in enumerate(rounds[11:], 12):
        print(f"Week {i} (Division Matchups):")
        for match in round_matches:
            print(f"  {match[0]} vs {match[1]}")
        print()


def add_division_matchups(rounds: List[List[Tuple[str, str]]]) -> List[List[Tuple[str, str]]]:
    """
    Add division matchups after the initial round-robin schedule, where each player plays one game against each division opponent.
    
    Args:
        rounds: List of rounds from the round-robin schedule
        
    Returns:
        Updated list of rounds with division matchups added
    """
    # Define the divisions
    division1 = ["Kyle", "Trevor", "Josh K", "Anthony"]
    division2 = ["Matt Rob", "Smitty", "Jake", "Josh L"]
    division3 = ["Devan", "Nikhil", "Greg", "Alec"]
    
    # Create division matchups - each player plays one game against each division opponent
    division_matchups = []
    
    # For each division, create all possible matchups within the division
    for div in [division1, division2, division3]:
        for i in range(len(div)):
            for j in range(i + 1, len(div)):
                division_matchups.append((div[i], div[j]))
    
    # Shuffle all division matchups
    random.shuffle(division_matchups)
    
    # We need to add 3 rounds of division matchups (each with 6 games)
    # Each round will have 6 games (half of the 12 players playing)
    for _ in range(3):
        round_matches = []
        
        # Track players who have already been scheduled for this round
        scheduled_players = set()
        
        # Add games to this round until we have 6 games or run out of matchups
        for match in division_matchups.copy():
            player1, player2 = match
            
            # Only add the match if neither player is already scheduled for this round
            if player1 not in scheduled_players and player2 not in scheduled_players:
                round_matches.append(match)
                scheduled_players.add(player1)
                scheduled_players.add(player2)
                division_matchups.remove(match)
                
                # If we have 6 games (all 12 players playing), move to next round
                if len(round_matches) == 6:
                    break
        
        # Add this round to the schedule
        rounds.append(round_matches)
    
    return rounds


def export_schedule_to_csv(rounds: List[List[Tuple[str, str]]], filename: str = "schedule.csv") -> None:
    """
    Export the schedule to a CSV file.
    
    Args:
        rounds: List of rounds, where each round is a list of matches
        filename: Name of the CSV file to create
    """
    with open(filename, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        
        # Write header row
        header = ["Week", "Type", "Home", "Away"]
        writer.writerow(header)
        
        # Write regular round-robin rounds (first 11 rounds)
        for i, round_matches in enumerate(rounds[:11], 1):
            for match in round_matches:
                writer.writerow([i, "Regular", match[0], match[1]])
        
        # Write division matchups rounds (last 3 rounds)
        for i, round_matches in enumerate(rounds[11:], 12):
            for match in round_matches:
                writer.writerow([i, "Division", match[0], match[1]])
    
    print(f"\nSchedule exported to {filename}")


def main():
    """Main function to run the scheduler."""
    # Hard-coded list of 12 names
    names = [
        "Smitty", 
        "Alec", 
        "Greg", 
        "Anthony", 
        "Matt Rob", 
        "Josh K", 
        "Josh L", 
        "Trevor", 
        "Nikhil", 
        "Jake", 
        "Devan", 
        "Kyle"
    ]
    
    # Create the initial round-robin schedule
    rounds = create_round_robin_schedule(names)
    
    # Add division matchups
    rounds = add_division_matchups(rounds)
    
    # Print the complete schedule
    print_schedule(rounds)
    
    # Export schedule to CSV
    export_schedule_to_csv(rounds)
    
    # Print some statistics
    total_matches = sum(len(round_matches) for round_matches in rounds)
    print(f"Total weeks: {len(rounds)}")
    print(f"Total matches: {total_matches}")


if __name__ == "__main__":
    main()
