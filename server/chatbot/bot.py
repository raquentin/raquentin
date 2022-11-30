from chatterbot import ChatBot
from chatterbot.trainers import ListTrainer

bot = ChatBot("Dima")

trainer = ListTrainer(bot)
parsedTrainingData = []
with open("training_data.txt") as f:
    for line in f:
        parsedTrainingData.append(line)
print(parsedTrainingData)
f.close()
trainer.train(parsedTrainingData)

stop_strings = (":q!")

while True:
    query = input("> ")
    if query in stop_strings:
        break
    else:
        print(f"Bot: {bot.get_response(query)}")