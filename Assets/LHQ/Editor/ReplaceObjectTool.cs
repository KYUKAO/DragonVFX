using UnityEngine;
using UnityEditor;

public class ReplaceObjectTool : EditorWindow
{
    private Object objectB;
    private GameObject[] selectedObjects;
    private GameObject[] copiedObjectsB;

    [MenuItem("Tools/Replace Object Tool")]
    private static void ShowWindow()
    {
        ReplaceObjectTool window = GetWindow<ReplaceObjectTool>();
        window.titleContent = new GUIContent("Replace Object Tool");
        window.Show();
    }

    private void OnGUI()
    {
        GUILayout.Label("Drag Object B from another scene here:", EditorStyles.boldLabel);
        objectB = EditorGUILayout.ObjectField(objectB, typeof(Object), true);

        if (GUILayout.Button("Replace Selected Objects"))
        {
            ReplaceObjects();
        }

        if (GUILayout.Button("Undo"))
        {
            UndoReplace();
        }

        if (GUILayout.Button("Redo"))
        {
            RedoReplace();
        }
    }

    private void ReplaceObjects()
    {
        if (Selection.gameObjects == null || Selection.gameObjects.Length == 0 || objectB == null)
        {
            Debug.LogWarning("No objects selected or Object B is not assigned!");
            return;
        }

        selectedObjects = Selection.gameObjects;

        // Record the current state for Undo
        Undo.RecordObjects(selectedObjects, "Replace Objects");

        // Replace selected objects with object B
        foreach (var obj in selectedObjects)
        {
            GameObject newObject = null;
            if (PrefabUtility.IsPartOfPrefabAsset(objectB))
            {
                newObject = (GameObject)PrefabUtility.InstantiatePrefab(objectB);
                newObject.transform.position = obj.transform.position;
                newObject.transform.rotation = obj.transform.rotation;
                newObject.transform.localScale = obj.transform.localScale;
            }
            else if (objectB is GameObject)
            {
                newObject = Instantiate((GameObject)objectB, obj.transform.position, obj.transform.rotation);
                newObject.transform.localScale = obj.transform.localScale;
            }
            else
            {
                Debug.LogWarning("Invalid object type for replacement!");
                return;
            }

            if (newObject != null)
            {
                newObject.transform.parent = obj.transform.parent;
                newObject.transform.SetSiblingIndex(obj.transform.GetSiblingIndex());
                Undo.RegisterCreatedObjectUndo(newObject, "Replace Objects");
                DestroyImmediate(obj);
            }
        }
    }

    private void UndoReplace()
    {
        // Undo the replace operation
        if (selectedObjects != null)
        {
            foreach (var obj in selectedObjects)
            {
                GameObject newObject = null;
                if (PrefabUtility.IsPartOfPrefabInstance(obj))
                {
                    newObject = (GameObject)PrefabUtility.GetCorrespondingObjectFromSource(obj);
                    newObject = PrefabUtility.InstantiatePrefab(newObject) as GameObject;
                    newObject.transform.position = obj.transform.position;
                    newObject.transform.rotation = obj.transform.rotation;
                    newObject.transform.localScale = obj.transform.localScale;
                }
                else
                {
                    newObject = Instantiate(obj, obj.transform.position, obj.transform.rotation);
                    newObject.transform.localScale = obj.transform.localScale;
                }

                if (newObject != null)
                {
                    newObject.transform.parent = obj.transform.parent;
                    newObject.transform.SetSiblingIndex(obj.transform.GetSiblingIndex());
                    Undo.RegisterCreatedObjectUndo(newObject, "Undo Replace Objects");
                    DestroyImmediate(obj);
                }
            }
        }
    }

    private void RedoReplace()
    {
        // Redo the replace operation
        ReplaceObjects();
    }
}

