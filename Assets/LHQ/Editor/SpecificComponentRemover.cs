using System;
using UnityEngine;
using UnityEditor;

public class SpecificComponentRemover : EditorWindow
{
    [MenuItem("Tools/Remove All AudioSources From Selected %#a")]
    static void RemoveAllAudioSources()
    {
        foreach (GameObject obj in Selection.gameObjects)
        {
            AudioSource[] audioSources = obj.GetComponentsInChildren<AudioSource>(true);
            foreach (AudioSource audioSource in audioSources)
            {
                DestroyImmediate(audioSource);
            }
        }
    }

    [MenuItem("Tools/Remove All Colliders From Selected %#c")]
    static void RemoveAllColliders()
    {
        foreach (GameObject obj in Selection.gameObjects)
        {
            Collider[] colliders = obj.GetComponentsInChildren<Collider>(true);
            foreach (Collider collider in colliders)
            {
                DestroyImmediate(collider);
            }
        }
    }
}