-record(user, {name,
               room=nil}). % current room of the user, or `nil`

-record(userroom, {type, % can be either `public` or `private`
                   name}).

-record(room, {owner,             % the room creator
               type=public,       % either `public` or `private`
               members=[],        % users that are invited to the room
               participants=[]}). % users currently in room

