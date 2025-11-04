defmodule Quiz do
  @moduledoc """
  A markdown-driven quiz system for Livebook using Kino.

  Parses markdown quiz definitions and renders interactive multiple choice quizzes.
  Quiz content is hidden inside HTML comments to keep answers invisible on the page.

  ## Usage

      markdown = \"\"\"
      ## Question 1
      What is a process in BEAM?

      - [ ] A thread
      - [x] A lightweight concurrent unit
      - [ ] A function

      ## Question 2
      How are BEAM processes scheduled?

      - [ ] By the OS
      - [x] By the BEAM VM scheduler
      - [ ] Randomly
      \"\"\"

      Quiz.render(markdown)
  """

  defmodule Question do
    @moduledoc false
    defstruct [:number, :text, :answers, :correct_indices]
  end

  defmodule Answer do
    @moduledoc false
    defstruct [:text, :correct]
  end

  @doc """
  Renders an interactive quiz from markdown content.

  The markdown format uses:
  - `## Question N` headers to denote questions
  - `- [x]` for the correct answer
  - `- [ ]` for incorrect answers
  """
  def render(markdown) do
    questions = parse_markdown(markdown)
    render_quiz(questions)
  end

  @doc """
  Renders quizzes from the current livebook file.

  Searches for markdown sections inside `<!-- quiz -->` comment blocks
  and renders all questions found within those sections.

  ## Usage in a Livebook

      # My Lesson

      ## Quiz 1
      <!--
      quiz

      ### Question 1
      What is a process?

      - [ ] A thread
      - [x] A lightweight unit
      -->

      ## Quiz 2
      <!--
      quiz

      ### Question 1
      What is OTP?

      - [x] Open Telecom Platform
      - [ ] Other stuff
      -->

      ## Render quizzes

      ```elixir
      # Render first quiz only
      Quiz.render_from_file(__DIR__ <> "/my-notebook.livemd", quiz: 1)

      # Render second quiz only
      Quiz.render_from_file(__DIR__ <> "/my-notebook.livemd", quiz: 2)

      # Render all quizzes combined (default)
      Quiz.render_from_file(__DIR__ <> "/my-notebook.livemd")
      ```

  Options:
  - `quiz: :all` (default) - Combines all quiz sections into one quiz
  - `quiz: N` - Renders only the Nth quiz (1-indexed, based on order of `<!-- quiz -->` markers)
  """
  def render_from_file(livebook_path, opts \\ []) do
    quiz_number = Keyword.get(opts, :quiz, :all)

    case File.read(livebook_path) do
      {:ok, content} ->
        quiz_sections = extract_quiz_sections_list(content)

        case quiz_number do
          :all ->
            # Combine all quiz sections
            quiz_markdown = Enum.join(quiz_sections, "\n\n")
            render(quiz_markdown)

          n when is_integer(n) ->
            # Render specific quiz
            case Enum.at(quiz_sections, n - 1) do
              nil ->
                Kino.Markdown.new("**Error: Quiz #{n} not found. Found #{length(quiz_sections)} quiz(zes) in file.**")

              quiz_markdown ->
                render(quiz_markdown)
            end
        end

      {:error, reason} ->
        Kino.Markdown.new("**Error: Could not read file #{livebook_path}** (#{inspect(reason)})")
    end
  end

  @doc """
  Extracts quiz sections from livebook markdown as a list.

  Looks for sections marked with `<!-- quiz -->` comment.
  Returns a list of markdown strings, one for each quiz section.
  Filters out empty sections.
  """
  def extract_quiz_sections_list(livebook_content) do
    livebook_content
    |> String.split("\n")
    |> find_quiz_sections([])
    |> Enum.filter(fn section ->
      # Only keep sections that have actual content (not just whitespace)
      String.trim(section) != ""
    end)
  end

  @doc """
  Extracts quiz sections from livebook markdown.

  Looks for sections marked with `<!-- quiz -->` comment.
  Combines all sections into one markdown string.
  """
  def extract_quiz_sections(livebook_content) do
    extract_quiz_sections_list(livebook_content)
    |> Enum.join("\n\n")
  end

  defp find_quiz_sections([], acc), do: Enum.reverse(acc)

  defp find_quiz_sections(lines, acc) do
    case find_next_quiz_section(lines) do
      {:ok, quiz_content, remaining} ->
        find_quiz_sections(remaining, [quiz_content | acc])

      :none ->
        Enum.reverse(acc)
    end
  end

  defp find_next_quiz_section(lines) do
    # Find lines that start with <!-- (ignoring leading whitespace)
    # This avoids matching <!-- inside code blocks or inline text
    case Enum.find_index(lines, fn line ->
      String.trim_leading(line) |> String.starts_with?("<!--")
    end) do
      nil ->
        :none

      idx ->
        comment_start_line = Enum.at(lines, idx)

        # Check if "quiz" is on the same line or the next line
        has_quiz_on_same_line = String.contains?(comment_start_line, "quiz")
        remaining_after_comment_start = Enum.drop(lines, idx + 1)

        # Check next line for "quiz" if not on same line
        has_quiz_on_next_line = case remaining_after_comment_start do
          [next_line | _] -> String.trim(next_line) =~ ~r/^\s*quiz\s*$/
          [] -> false
        end

        cond do
          # Not a quiz comment, skip it
          !has_quiz_on_same_line && !has_quiz_on_next_line ->
            # Continue searching after this comment
            find_next_quiz_section(remaining_after_comment_start)

          # Quiz marker found - extract content
          true ->
            # Determine where quiz content starts
            content_start_offset = if has_quiz_on_next_line, do: 1, else: 0
            content_lines = Enum.drop(lines, idx + 1 + content_start_offset)

            # Find the closing -->
            case Enum.find_index(content_lines, &String.contains?(&1, "-->")) do
              nil ->
                # No closing comment found, treat as empty
                {:ok, "", content_lines}

              close_idx ->
                # Extract all lines between quiz marker and -->
                quiz_lines = Enum.take(content_lines, close_idx)

                # Check if closing --> is on same line as last content
                closing_line = Enum.at(content_lines, close_idx)
                last_line_content = String.replace(closing_line, ~r/-->.*/, "") |> String.trim()

                # Add the last line content if it has any
                all_quiz_lines = if last_line_content != "" do
                  quiz_lines ++ [last_line_content]
                else
                  quiz_lines
                end

                quiz_content = Enum.join(all_quiz_lines, "\n")
                rest = Enum.drop(content_lines, close_idx + 1)

                {:ok, quiz_content, rest}
            end
        end
    end
  end


  @doc """
  Parses markdown into a list of Question structs.
  """
  def parse_markdown(markdown) do
    markdown
    |> String.split("\n")
    |> parse_lines([])
    |> Enum.reverse()
  end

  defp parse_lines([], questions), do: questions

  defp parse_lines(lines, questions) do
    case find_next_question(lines) do
      {:ok, question, remaining_lines} ->
        parse_lines(remaining_lines, [question | questions])

      :none ->
        questions
    end
  end

  defp find_next_question(lines) do
    case find_question_header(lines) do
      {:ok, number, text_lines, remaining} ->
        {question_text, answer_lines, rest} = extract_question_parts(text_lines, remaining)
        answers = parse_answers(answer_lines)

        # Find all correct answer indices
        correct_indices = answers
          |> Enum.with_index()
          |> Enum.filter(fn {answer, _idx} -> answer.correct end)
          |> Enum.map(fn {_answer, idx} -> idx end)

        question = %Question{
          number: number,
          text: String.trim(question_text),
          answers: answers,
          correct_indices: correct_indices
        }

        {:ok, question, rest}

      :none ->
        :none
    end
  end

  defp find_question_header(lines) do
    Enum.reduce_while(lines, {0, []}, fn line, {index, acc} ->
      trimmed = String.trim(line)

      cond do
        # Match ## Question N or ### Question N or #### Question N
        is_question_header?(trimmed) ->
          number = extract_question_number(trimmed)
          {:halt, {:found, number, index, acc}}

        true ->
          {:cont, {index + 1, acc ++ [line]}}
      end
    end)
    |> case do
      {:found, number, index, _before} ->
        remaining = Enum.drop(lines, index + 1)
        {:ok, number, remaining, remaining}

      _ ->
        :none
    end
  end

  defp is_question_header?(line) do
    # Match headers like "## Question 1", "### Question 2", etc.
    String.match?(line, ~r/^##+ Question \d+/) or
    String.match?(line, ~r/^##+ .*Question.*/)
  end

  defp extract_question_number(header) do
    case Regex.run(~r/Question\s+(\d+)/, header) do
      [_, num] -> String.to_integer(num)
      _ -> 1
    end
  end

  defp extract_question_parts(_text_lines, all_lines) do
    {question_lines, rest} =
      Enum.split_while(all_lines, fn line ->
        trimmed = String.trim(line)
        !is_answer_line?(trimmed)
      end)

    {answer_lines, remaining} =
      Enum.split_while(rest, fn line ->
        trimmed = String.trim(line)
        is_answer_line?(trimmed) || String.trim(line) == ""
      end)

    question_text = Enum.join(question_lines, "\n")
    {question_text, answer_lines, remaining}
  end

  defp is_answer_line?(trimmed) do
    String.starts_with?(trimmed, "- [") || String.starts_with?(trimmed, "* [")
  end

  defp parse_answers(lines) do
    lines
    |> Enum.filter(fn line ->
      trimmed = String.trim(line)
      is_answer_line?(trimmed)
    end)
    |> Enum.map(&parse_answer/1)
  end

  defp parse_answer(line) do
    trimmed = String.trim(line)

    cond do
      String.starts_with?(trimmed, "- [x]") or String.starts_with?(trimmed, "* [x]") ->
        text = trimmed
               |> String.trim_leading("- [x]")
               |> String.trim_leading("* [x]")
               |> String.trim()
        %Answer{text: text, correct: true}

      String.starts_with?(trimmed, "- [X]") or String.starts_with?(trimmed, "* [X]") ->
        text = trimmed
               |> String.trim_leading("- [X]")
               |> String.trim_leading("* [X]")
               |> String.trim()
        %Answer{text: text, correct: true}

      String.starts_with?(trimmed, "- [ ]") or String.starts_with?(trimmed, "* [ ]") ->
        text = trimmed
               |> String.trim_leading("- [ ]")
               |> String.trim_leading("* [ ]")
               |> String.trim()
        %Answer{text: text, correct: false}

      true ->
        %Answer{text: trimmed, correct: false}
    end
  end

  defp render_quiz(questions) when length(questions) == 0 do
    Kino.Markdown.new("**No questions found in the provided markdown.**")
  end

  defp render_quiz(questions) do
    # Store current checkbox state in an agent
    {:ok, state_agent} = Agent.start_link(fn -> %{} end)

    # Create widgets with headers and checkboxes
    # Subscribe to each checkbox individually with field name in closure
    all_widgets = questions
      |> Enum.flat_map(fn q ->
        # Question header
        header = Kino.Markdown.new("**Question #{q.number}**: #{q.text}\n")

        # Create checkboxes and subscribe to each
        checkboxes = q.answers
          |> Enum.with_index()
          |> Enum.map(fn {answer, idx} ->
            field_name = String.to_atom("q#{q.number}_a#{idx}")
            input = Kino.Input.checkbox(answer.text, default: false)

            # Subscribe directly to this specific checkbox
            Kino.listen(input, fn event ->
              value = case event do
                %{value: v} -> v
                v when is_boolean(v) -> v
                _ -> event
              end
              Agent.update(state_agent, &Map.put(&1, field_name, value))
            end)

            input
          end)

        [header | checkboxes]
      end)

    # Create submit button
    submit_button = Kino.Control.button("Submit Quiz")

    # Create frame for results
    frame = Kino.Frame.new()
    Kino.Frame.render(frame, Kino.Markdown.new(""))

    # Listen to submit button only
    Kino.listen(submit_button, fn _event ->
      answers = Agent.get(state_agent, & &1)
      score = calculate_score(answers, questions)
      result_markdown = format_results(score, questions, answers)
      Kino.Frame.render(frame, Kino.Markdown.new(result_markdown))
    end)

    # Layout
    quiz_section = Kino.Layout.grid(all_widgets, columns: 1)
    Kino.Layout.grid([quiz_section, submit_button, frame], boxed: true, gap: 16)
  end

  defp calculate_score(answers, questions) do
    questions
    |> Enum.reduce(0, fn q, score ->
      # Get all checked answers for this question
      selected_indices = q.answers
        |> Enum.with_index()
        |> Enum.filter(fn {_answer, idx} ->
          field_name = String.to_atom("q#{q.number}_a#{idx}")
          Map.get(answers, field_name, false) == true
        end)
        |> Enum.map(fn {_answer, idx} -> idx end)
        |> Enum.sort()

      # Check if selected answers match correct answers exactly
      correct_indices_sorted = Enum.sort(q.correct_indices)

      if selected_indices == correct_indices_sorted do
        score + 1
      else
        score
      end
    end)
  end

  defp format_results(score, questions, answers) do
    total = length(questions)
    percentage = round(score / total * 100)

    emoji =
      cond do
        percentage == 100 -> "🎉"
        percentage >= 80 -> "✅"
        percentage >= 60 -> "👍"
        true -> "📚"
      end

    header = """
    #{emoji} **Quiz Results**

    Score: **#{score}/#{total}** (#{percentage}%)

    ---

    """

    details =
      questions
      |> Enum.map(fn q ->
        # Get selected answer indices
        selected_indices = q.answers
          |> Enum.with_index()
          |> Enum.filter(fn {_answer, idx} ->
            field_name = String.to_atom("q#{q.number}_a#{idx}")
            Map.get(answers, field_name, false) == true
          end)
          |> Enum.map(fn {_answer, idx} -> idx end)
          |> Enum.sort()

        correct_indices_sorted = Enum.sort(q.correct_indices)
        correct = selected_indices == correct_indices_sorted

        status = if correct, do: "✓", else: "✗"
        color = if correct, do: "🟢", else: "🔴"

        # Format selected answers
        selected_texts = selected_indices
          |> Enum.map(&Enum.at(q.answers, &1).text)

        selected_display = if Enum.empty?(selected_texts) do
          "(none)"
        else
          selected_texts |> Enum.map(&"- #{&1}") |> Enum.join("\n")
        end

        # Format correct answers
        correct_texts = q.correct_indices
          |> Enum.map(&Enum.at(q.answers, &1).text)

        correct_display = correct_texts |> Enum.map(&"- #{&1}") |> Enum.join("\n")

        result = """
        #{color} **Question #{q.number}** #{status}

        #{q.text}

        Your answer(s):
        #{selected_display}
        """

        if correct do
          result
        else
          result <> "\nCorrect answer(s):\n#{correct_display}\n"
        end
      end)
      |> Enum.join("\n---\n\n")

    header <> details
  end
end
