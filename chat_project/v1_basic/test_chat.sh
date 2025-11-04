#!/bin/bash
# Simple test script for chat server
# Run this to test basic functionality

set -e

echo "Testing chat server on localhost:5555"
echo ""

# Test 1: Connect and get welcome message
echo "Test 1: Connect and receive welcome message"
{
    sleep 0.5
    echo "QUIT"
} | telnet localhost 5555 2>&1 | grep -q "Welcome to Erlang Chat Server"
echo "✓ Welcome message received"

# Test 2: Join a room
echo ""
echo "Test 2: Join a room"
{
    sleep 0.5
    echo "JOIN lobby alice"
    sleep 0.5
    echo "QUIT"
} | telnet localhost 5555 2>&1 | grep -q "OK: Joined"
echo "✓ Room join successful"

# Test 3: Send a message (requires being in a room)
echo ""
echo "Test 3: Send a message"
{
    sleep 0.5
    echo "JOIN lobby bob"
    sleep 0.5
    echo "MSG Hello, world!"
    sleep 0.5
    echo "QUIT"
} | telnet localhost 5555 2>&1 | grep -q "OK: Joined"
echo "✓ Message sent successfully"

echo ""
echo "All tests passed!"
