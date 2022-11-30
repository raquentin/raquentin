stop_strings = (":q!")

while True:
    with open("training_data.txt", 'a') as f:
        message = input("> ")
        if message in stop_strings:
            break
        f.write(message)
f.close()