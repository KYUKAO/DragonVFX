using UnityEngine;
using UnityEngine.Playables;

[RequireComponent(typeof(PlayableDirector))]
public class LoopTimeline : MonoBehaviour
{
    private PlayableDirector director;

    void Start()
    {
        // 获取PlayableDirector组件
        director = GetComponent<PlayableDirector>();
        
        // 注册Timeline播放完毕时的回调函数
        director.stopped += OnPlayableDirectorStopped;
    }

    // 当PlayableDirector停止播放时调用
    private void OnPlayableDirectorStopped(PlayableDirector aDirector)
    {
        if (director == aDirector)
        {
            // 重新开始播放Timeline
            director.Play();
        }
    }

    void OnDestroy()
    {
        // 清理，避免内存泄漏
        director.stopped -= OnPlayableDirectorStopped;
    }
}