from flask import Flask, request, Response
import yt_dlp
import json
import datetime

app = Flask(__name__)

@app.route('/get_chapters', methods=['GET'])
def get_chapters():
    youtube_url = request.args.get('url')
    ydl_opts = {
        'dumpjson': True,
        'quiet': True
    }
    with yt_dlp.YoutubeDL(ydl_opts) as ydl:
        info = ydl.extract_info(youtube_url, download=False)
    chapters = info.get('chapters', [])
    output = ""
    for chapter in chapters:
        start_time = int(chapter['start_time'])
        title = chapter['title']
        time = str(datetime.timedelta(seconds=start_time))
        output += f"{time} {title}\n"
    return Response(output, mimetype='text/plain')

if __name__ == "__main__":
    app.run(debug=True)

