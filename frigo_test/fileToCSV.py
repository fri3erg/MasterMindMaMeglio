import csv
import sys

def parse_trivia_file(input_path):
    with open(input_path, encoding='utf-8') as f:
        lines = [line.strip() for line in f if line.strip()]

    questions = []
    i = 0
    while i < len(lines):
        if lines[i].startswith("#Q"):
            question = lines[i][2:].strip()
            i += 1
            correct = lines[i][1:].strip() if lines[i].startswith("^") else ""
            i += 1
            options = []
            while i < len(lines) and lines[i][:2] in {"A ", "B ", "C ", "D ", "E ", "F "}:
                options.append(lines[i][2:].strip())
                i += 1
            questions.append((question, correct, options))
        else:
            i += 1
    return questions

def write_csv(data, output_path):
    with open(output_path, mode='w', encoding='utf-8', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Question', 'Correct Answer', 'Correct Index', 'Option A', 'Option B', 'Option C', 'Option D'])
        for question, correct, options in data:
            index = next((i + 1 for i, opt in enumerate(options) if opt == correct), '')
            placeholder = "N/A"
            row = [question, correct, index] + options + [placeholder] * (4 - len(options))
            writer.writerow(row)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python script.py input_file output_file.csv")
        sys.exit(1)

    input_file = sys.argv[1]
    output_file = sys.argv[2]

    trivia_data = parse_trivia_file(input_file)
    write_csv(trivia_data, output_file)
