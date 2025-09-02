import QtQuick.Controls 2.12
import QtQuick.Layouts 1.15

ApplicationWindow {
    visible: true

    ColumnLayout {
        anchors.fill: parent

        Label {
            text: ctx_text
        }

        Button {
            text: "my button"
        }
    }
}
